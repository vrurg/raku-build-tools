#!/usr/bin/env raku
# use Grammar::Tracer;
use lib 'lib';
use Async::Workers;

my $VERBOSE = False;
my $MAIN-MOD = %*ENV<MAIN_MOD>;
my $MODULE := Mu;
my $MOD-VERSION;
my $URL;

grammar MyPOD {
    token TOP {
        [
            <pod>
            || <dummy>
        ]+
    }

    token dummy {
        [ <!before <.pod-begin>> . && . ]+
    }

    token pod-begin {
        ^^ '=begin' \h
    }

    token pod-start ( $pod-kw is rw ) {
        <pod-begin> \h* $<pod-kw>=\w+ { $pod-kw = ~$/<pod-kw> } \h* $$
    }

    token pod-end ( $pod-kw ) {
        ^^ '=end' \h+ $pod-kw \h* $$
    }

    token pod {
        :my $pod-kw;
        <pod-start( $pod-kw )>
        [
            || <pod-link>
            || <pod-text>
        ]+
        <pod-end( $pod-kw )>
    }

    token pod-text {
        .+? <?before [[L | TYPE] '<'] || [^^ '=end']>
    }

    proto token pod-link {*}
    multi token pod-link:sym<mod-url> {
        'L<' <link-module> '|' <link-url> '>'
    }
    multi token pod-link:sym<mod-only> {
        'L<' <link-module> '>'
    }
    multi token pod-link:sym<raku-type> {
        'TYPE<' <link-module> '>'
    }

    token link-text {
        .+? <?before [ '|' | '>' ]>
    }

    token link-module {
          'C<' <link-module-name> '>'
        | <link-module-name>
    }
    token link-module-name {
        [ <.alnum>+ ] ** 1..* % '::'
    }

    token link-url {
        $<link-prefix>=[ 'https://github.com/' <.url-char>+? '/blob/v' ] <version> $<link-suffix>=[ '/' <.url-char>+ ]
    }

    token url-char {
        <!before '>'> . && .
    }

    token version {
        [\d+] ** 3 % '.'
    }
}

class MyPOD-Actions {
    has Bool $.replaced is rw = False;
    has $!ver-str = ~$MOD-VERSION;

    method version ($/) {
        $.replaced ||= Version.new( ~$/ ) ≠ $MOD-VERSION;
        make $!ver-str;
    }

    method pod-link:sym<mod-only> ( $/ ) {
        my $link-mod = $<link-module>.made;
        my $link-url;
        if $<link-module><link-module-name>.Str.starts-with($MAIN-MOD) {
            my $link-path = $link-mod.subst('::', '/', :g);
            $link-url = $URL ~ '/blob/v' ~ $!ver-str ~ '/docs/md/' ~ $link-path ~ '.md';
        }
        else {
            $link-url = 'https://modules.raku.org/dist/' ~ $<link-module><link-module-name>;
        }
        $.replaced = True;
        make 'L<' ~ $<link-module> ~ '|' ~ $link-url ~ '>';
    }

    method pod-link:sym<raku-type>($/) {
        my $link-mod = $<link-module>.made;
        make 'L<C<' ~ $link-mod
             ~ '>|https://docs.raku.org/type/'
             ~ $link-mod ~ '>';
        $.replaced = True;
    }

    method link-module($/) {
        make $<link-module-name>
    }

    method link-url ($/) {
        make $<link-prefix> ~ $<version>.made ~ $<link-suffix>;
    }

    method FALLBACK ($name, $m) {
        $m.make(
            $m.chunks.map( { given .value { .?made // ~$_ } } ).join
        );
    }
}

sub patch-a-doc(Str:D $pod-file, :$force? --> Str) {
    say "===> Updating ", $pod-file if $VERBOSE;
    my Bool $backup = False;
    my $src = $pod-file.IO.slurp;
    my $actions = MyPOD-Actions.new;
    my $res = MyPOD.parse( $src, :$actions );

    die "Failed to parse the source" unless $res;

    if $force || $actions.replaced {
        if $backup {
            my $idx = 0;
            my $bak-file = $pod-file ~ ".bk";
            while $bak-file.IO.e {
                $bak-file = $pod-file ~ (++$idx).fmt(".%02d.bk");
            }
            $pod-file.IO.rename( $bak-file );
        }

        $pod-file.IO.spurt( $res.made );
        say "===> Updated ", $pod-file;
    }
    $pod-file
}

my $mdl = Lock.new;
sub make-dest($src is copy, $base is copy, $fmt, :$output) {
    $mdl.protect: {
        if $VERBOSE {
            $src = $src.IO.absolute;
            $base = $base.IO.absolute;
            my @d = $*SPEC.splitdir($src.IO.relative($base))[1 ..*];
            note "___ from $src base:$base ---> ", ~$src.IO.relative($base);
            note ">>>> [$fmt] ", @d.join(", "), " ---> ", my $res = $*SPEC.catdir('docs', $fmt,
            |$*SPEC.splitdir($src.IO.relative($base))[1 ..*]);
            note "!!! $src --- ", $res.IO.extension($fmt);
        }
        ($output || $*SPEC.catdir('docs', $fmt, |$*SPEC.splitdir($src.IO.relative($base))[1 ..*])).IO.extension($fmt);
    }
}

sub invoke-raku($src, $dst, $fmt) {
    my $dfh = $dst.IO.open(:w);
    my @cmd = ~$*EXECUTABLE, '-Ilib', '--doc=' ~ $fmt, $src;
    say @cmd.join(" "), ' >', ~$dst if $VERBOSE;
    my $p = run |@cmd, :out($dst.IO.open(:w));
}

proto gen-fmt(Str:D, $, $, *%) {*}
multi gen-fmt('md', $src, $base, :$output) {
    my $md-dest = make-dest($src, $base, 'md', :$output);
    if !$md-dest.e || $src.IO.modified > $md-dest.modified {
        say "===> Generating ",
            |($VERBOSE ?? $src ~ " --> " !! Empty),
            ~$md-dest;
        invoke-raku $src, $md-dest, 'Markdown';
    }
}
multi gen-fmt('html', $src, $base, :$output) {
    my $html-dest = make-dest($src, $base, 'html', :$output);
    if !$html-dest.e || ($src.IO.modified > $html-dest.modified) {
        say "===> Generating ", ~$html-dest;
        invoke-raku $src, $html-dest, 'HTML';
    }
}

sub prepare-module {
    require ::($MAIN-MOD);
    require ::($MAIN-MOD ~ '::META');
    $MODULE := ::($MAIN-MOD);
    $MOD-VERSION = $MODULE.^ver;
    my %meta = ::('&' ~ $MAIN-MOD ~ '::META::META6').();
    $URL = S/\.git$// with %meta<source-url>;
}

sub gen-doc(+@pod-files, :$module, :$base, :$output, :$force, :%into) {
    $MAIN-MOD = $_ with $module;
    prepare-module;
    my $wm = Async::Workers.new(:max-workers($*KERNEL.cpu-cores));
    for @pod-files -> $pod-file {
        say "??? $pod-file" if $VERBOSE;
        $wm.do-async: {
            CATCH { note $_; note ~.bt; exit 1; }
            patch-a-doc($pod-file, :$force);
            for %into.keys -> $fmt {
                next unless %into{$fmt};
                $wm.do-async: {
                    gen-fmt $fmt, $pod-file, $base, :$output;
                }
            }
        }
    }
    await $wm;
    $wm.shutdown;
}

multi MAIN (+@pod-files, Str :m(:module($module))!, Str() :$base = $*CWD, Bool :v(:verbose($verbose)) = False, Bool :f(:force($force)) = False,
            Bool :$md = False, Bool :$html = False) {
    $VERBOSE = $verbose;
    my @psorted = @pod-files.race.map( { [$_, .IO.s] } ).sort({ @^b[1] cmp @^a[1] }).map({.[0]});
    gen-doc(@psorted, :$module, :$force, :$base, :into{ :$md, :$html });
}
multi MAIN (Str:D $pod-file, Str :m(:module($module))!, Str() :$base = $*CWD, Str :o(:output($output))?,
            Bool :v(:verbose($verbose)) = False, Bool :f(:force($force)) = False,
            Bool :$md = False, Bool :$html = False ) {
    $VERBOSE = $verbose;
    gen-doc($pod-file, :$module, :$output, :$base, :into{ :$md, :$html });
}
