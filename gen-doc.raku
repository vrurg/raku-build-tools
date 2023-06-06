#!/usr/bin/env raku
# use Grammar::Tracer;
use Async::Workers;
use URI;

my $VERBOSE = False;
my $MAIN-MOD = %*ENV<MAIN_MOD>;
my $MODULE := Mu;
my $BASE;
my $MOD-VERSION;
my $FORCE = False;
my $URL;

grammar MyPOD {
    token TOP {
        [<pod> || <dummy>]+
    }

    token dummy {
        [<!before <.pod-begin>> . && .]+
    }

    token pod-begin {
        ^^ '=begin' \h
    }

    token pod-start ($pod-kw is rw) {
        <pod-begin> \h* $<pod-kw>=\w+ { $pod-kw = ~$/<pod-kw> } \h* $$
    }

    token pod-end ($pod-kw) {
        ^^ '=end' \h+ $pod-kw \h* $$
    }

    token pod {
        :my $pod-kw;
        <pod-start($pod-kw)>
        [<pod-link> || <pod-text>]+
        <pod-end($pod-kw)>
    }

    token pod-text {
        .+? <?before [[L | TYPE | EXAMPLE] '<'] || [^^ '=end']>
    }

    proto token pod-link {*}
    multi token pod-link:sym<mod-url> {
        'L<' <link-text> '|' <link-url> '>'
    }
    multi token pod-link:sym<mod-only> {
        'L<' <link-module> '>'
    }
    multi token pod-link:sym<raku-type> {
        'TYPE<' <link-module> '>'
    }
    multi token pod-link:sym<example-script> {
        'EXAMPLE<' $<script>=[<.url-char>+] '>'
    }

    token link-text {
        .*? <?before '|'>
    }

    token link-module {
        'C<' <link-module-name> '>' | <link-module-name>
    }
    token link-module-name {
        $<lmn-type>=[ [<.alnum>+] ** 1..* % '::' ]
        $<lmn-smile>=[ ':D' || ':U' ]?
    }

    proto token link-url {*}
    multi token link-url:sym<full> {
        $<link-prefix>=['https://github.com/' <.url-char>+? '/blob/v'] <version> $<link-suffix>=['/' <.url-char>+]
    }
    multi token link-url:sym<rel> {
        <!before http s? '://'> && $<link>=[<.url-char>+]
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
    has $.pod-file;
    has $.dest-file-dir;

    submethod TWEAK {
        $!dest-file-dir //= IO::Spec::Unix.catfile($BASE, make-dest($!pod-file, $BASE, 'md')).IO.parent(1);
    }

    method version($/) {
        $.replaced ||= Version.new(~$/) ≠ $MOD-VERSION;
        make $!ver-str;
    }

    method pod-link:sym<mod-only>($/) {
        my $link-mod = $<link-module>.made;
        my $link-mod-text = ~$<link-module>;
        my $link-url;
        if $<link-module><link-module-name>.Str.starts-with($MAIN-MOD) {
            my $link-path = IO::Spec::Unix.catfile('docs', 'md', |$link-mod.split('::')) ~ '.md';
            $link-url = ~$link-path.IO.relative($!dest-file-dir);
        }
        else {
            $link-url = 'https://modules.raku.org/dist/' ~ $<link-module><link-module-name><lmn-type>;
        }
        $.replaced = True;
        make 'L<' ~ $<link-module> ~ '|' ~ $link-url ~ '>';
    }

    method pod-link:sym<mod-url>($/) {
        make 'L<' ~ $<link-text> ~ '|' ~ $<link-url>.made ~ '>';
    }

    method pod-link:sym<raku-type>($/) {
        my $link-mod = $<link-module>.made;
        make 'L<C<' ~ $<link-module><link-module-name> ~ '>|https://docs.raku.org/type/' ~ $link-mod ~ '>';
        $.replaced = True;
    }

    method pod-link:sym<example-script>($/) {
        my $script-path = IO::Spec::Unix.catfile('examples', ~$<script>);
        my $url-path = ~$script-path.IO.relative($!dest-file-dir);
        $.replaced = True;
        make 'L<I<' ~ $<script> ~ '>|' ~ $url-path ~ '>'
    }

    method link-module($/) {
        make ~$<link-module-name><lmn-type>
    }

    method link-url:sym<full> ($/) {
        make IO::Spec::Unix.catfile($BASE, $<link-suffix>).IO.relative($!dest-file-dir);
        $.replaced = True;
    }

    method link-url:sym<rel> ($/) {
        make ~$<link>
    }

    method FALLBACK ($name, $m) {
        $m.make($m.chunks.map({ given .value { .?made // ~$_ } }).join);
    }
}

sub patch-a-doc(Str:D $pod-file, :$output --> Str) {
    say "===> Updating ", $pod-file if $VERBOSE;
    my Bool $backup = False;
    my $src = $pod-file.IO.slurp;
    my $actions = MyPOD-Actions.new(
        :$pod-file,
        |(:dest-file-dir(.IO.parent(1)) with $output)
    );
    my $res = MyPOD.parse($src, :$actions);

    die "Failed to parse the source" unless $res;

    if $FORCE || $actions.replaced {
        if $backup {
            my $idx = 0;
            my $bak-file = $pod-file ~ ".bk";
            while $bak-file.IO.e {
                $bak-file = $pod-file ~ (++$idx).fmt(".%02d.bk");
            }
            $pod-file.IO.rename($bak-file);
        }

        $pod-file.IO.spurt($res.made);
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
    my $ddir = $dst.IO.parent(1);
    unless $ddir.d {
        $ddir.mkdir orelse .exception.throw;
    }
    my $dfh = $dst.IO.open(:w);
    $dfh.exception.rethrow unless $dfh;
    my @opts = (%*ENV<RAKU_OPTS> // "").split(" ");
    my @cmd = ~$*EXECUTABLE, '--doc=' ~ $fmt, |@opts, $src;
    say @cmd.join(" "), ' >', ~$dst if $VERBOSE;
    run |@cmd, :out($dfh);
}

proto gen-fmt(Str:D, $, $, *%) {*}
multi gen-fmt('md', $src, $base, :$output) {
    my $md-dest = make-dest($src, $base, 'md', :$output);
    if !$md-dest.e || $src.IO.modified > $md-dest.modified {
        say "===> Generating ", |($VERBOSE ?? $src ~ " --> " !! Empty), ~$md-dest;
        invoke-raku $src, $md-dest, 'Markdown';
    }
}
#multi gen-fmt('html', $src, $base, :$output) {
#    my $html-dest = make-dest($src, $base, 'html', :$output);
#    if !$html-dest.e || ($src.IO.modified > $html-dest.modified) {
#        say "===> Generating ", ~$html-dest;
#        invoke-raku $src, $html-dest, 'HTML';
#    }
#}

sub prepare-module {
    require ::($MAIN-MOD);
    my $meta_mod = %*ENV<META_MOD> // $MAIN-MOD ~ '::META';
    require ::($meta_mod);
    $MODULE := ::($MAIN-MOD);
    my %meta = ::('&' ~ $meta_mod ~ '::META6').();
    $MOD-VERSION = $MODULE.^ver // %meta<ver>;
    $URL = S/\.git$// with %meta<source-url>;
}

sub gen-doc(+@pod-files, :$module, :$base, :$output, :%into) {
    $MAIN-MOD = $_ with $module;
    $BASE = $base;
    prepare-module;
    my $wm = Async::Workers.new(:max-workers(1 || $*KERNEL.cpu-cores));
    for @pod-files -> $pod-file {
        say "??? $pod-file" if $VERBOSE;
        $wm.do-async: {
            CATCH {
                note $_;
                note ~.backtrace;
                exit 1;
            }
            patch-a-doc($pod-file, :$output);
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

multi MAIN (+@pod-files, Str :m(:module($module))!, Str() :$base = $*CWD, Bool :v(:verbose($verbose)),
            Bool :f(:force($force)) = False,
            Bool :$md = False, Bool :$html = False) {
    $VERBOSE = $_ with $verbose;
    $FORCE = $force;
    my @psorted = @pod-files.race.map({ [$_, .IO.s] }).sort({ @^b[1] cmp @^a[1] }).map({ .[0] });
    gen-doc(@psorted, :$module, :$base, :into{ :$md, :$html });
}
multi MAIN (Str:D $pod-file, Str :m(:module($module))!, Str() :$base = $*CWD, Str :o(:output($output))?,
            Bool :v(:verbose($verbose)), Bool :f(:force($force)) = False,
            Bool :$md = False, Bool :$html = False) {
    $VERBOSE = $_ with $verbose;
    $FORCE = $force;
    gen-doc($pod-file, :$module, :$output, :$base, :into{ :$md, :$html });
}
