#!/usr/bin/env raku
use v6.e.PREVIEW;
use experimental :rakuast;
use Async::Workers;
use URI;

my $VERBOSE = False;
my $MAIN-MOD = %*ENV<MAIN_MOD>;
my $MODULE := Mu;
my $BASE;
my $MOD-VERSION;
my $FORCE = False;
my $URL;

$*OUT.out-buffer = 0;
$*ERR.out-buffer = 0;

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
        [<macro> || <pod-text>]+
        <pod-end($pod-kw)>
    }

    token pod-text {
        .+? <?before <.macro-opener> || [^^ '=end']>
    }

    token macro {
        :my $*MACRO-LBRACE;
        :my $*MACRO-RBRACE;
        :my $*MACRO-KEYWORD;
        <macro-opener> <macro-arg> $*MACRO-RBRACE
    }

    token macro-opener {
        <.wb> $<macro-keyword>=[TYPE | EXAMPLE | TEST | FILE] <macro-lbrace>
        {
            $*MACRO-LBRACE = ~$<macro-lbrace> if DYNAMIC::<$*MACRO-LBRACE>:exists;
            $*MACRO-KEYWORD = ~$<macro-keyword> if DYNAMIC::<$*MACRO-KEYWORD>:exists;
        }
    }

    token macro-arg {
        .+? <?before $*MACRO-RBRACE>
    }

    proto token macro-lbrace {*}

    multi token macro-lbrace:sym«<» {
        '<'+ { $*MACRO-RBRACE = '>' x $/.chars if DYNAMIC::<$*MACRO-RBRACE>:exists }
    }

    multi token macro-lbrace:sym<«> {
        '«' { $*MACRO-RBRACE = '»' if DYNAMIC::<$*MACRO-RBRACE>:exists }
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

    method !link-file-from-top-level($/, $file, Str $subdir?) {
        my $file-path = IO::Spec::Unix.catfile(|($_ with $subdir), ~$file);
        unless $BASE.IO.add($file-path).e {
            note "!!! WARNING !!! Missing file '" ~ $file-path ~ "' in macro " ~ $*MACRO-KEYWORD;
        }
        my $url-path = ~$file-path.IO.relative($!dest-file-dir);
        $.replaced = True;
        my $lbr = $*MACRO-LBRACE || '«';
        my $rbr = $*MACRO-RBRACE || '»';
        make "L{$lbr}I{$lbr}" ~ $file ~ "$rbr|" ~ $url-path ~ $rbr
    }

    proto method unwrap-macro(|) {*}

    multi method unwrap-macro($/, 'TYPE', $type) {
        $.replaced = True;
        my $non-smiled = $type.ends-with(':D' | ':U') ?? $type.substr(0, *-2) !! $type;
        my $lbr = $*MACRO-LBRACE || '«';
        my $rbr = $*MACRO-RBRACE || '»';
        make "L{$lbr}C{$lbr}" ~ $type ~ $rbr ~ '|https://docs.raku.org/type/' ~ $non-smiled.split('::').join("/") ~ $rbr
    }

    multi method unwrap-macro($/, 'EXAMPLE', $file) {
        self!link-file-from-top-level($/, $file, 'examples');
    }

    multi method unwrap-macro($/, 'TEST', $file is copy) {
        my $tdir = $BASE.IO.add('t');
        unless $tdir.add($file).e {
            if $tdir.add($file ~ '.rakutest').e {
                $file ~= '.rakutest';
            }
        }
        self!link-file-from-top-level($/, $file, 't');
    }

    multi method unwrap-macro($/, 'FILE', $file) {
        self!link-file-from-top-level($/, $file);
    }

    multi method unwrap-macro($/, Str:D $kwd, |) {
        die "Unsupported macro '$kwd'"
    }

    method macro($/) {
        self.unwrap-macro($/, ~$<macro-opener><macro-keyword>, ~$<macro-arg>);
    }
    method FALLBACK ($name, $m) {
        $m.make($m.chunks.map({ given .value { .?made // ~$_ } }).join);
    }
}

my regex RxMod {
    $<module>=[ [ <.alpha> <.alnum>* ] ** 1..* % '::' ]
    [
        [ ':auth<' $<author>=[ .*? <?before '>'> ] '>' ]
        | [ ':ver<' $<version>=[ .*? <?before '>'> ] '>' ]
    ]*
    $<smile>=[ ':' D | U | _ ]?
}

sub tr-note(*@msg) {
    my $level = $*DOC-AST-LEVEL // 0;
    note @msg.map(*.gist).join.indent($level*2);
}

role ASTContext {
    method checkin {...}
    method checkout {...}
}

class ASTLink does ASTContext {
    has $.is-mod-link = True;
    has $.text-node;

    method checkin {
        my $cur-node = $*DOC-NODE;
        $!is-mod-link &&= $cur-node ~~ RakuAST::Doc::Markup && $cur-node.atoms.elems < 2;
    }

    method checkout {
        return unless $!is-mod-link;

        my $cur-node = $*DOC-NODE;
        $!is-mod-link &&= @*DOC-NODE-CHILDREN < 2;

        if $!is-mod-link && $cur-node.letter eq 'C' {
            if $cur-node.atoms.head ~~ &RxMod {
                $!text-node = $cur-node;
            }
        }
    }
}

class ASTContextStack {
    has ASTContext @.stack;
    method enter-ctx(ASTContext:D $ctx, &code) is raw {
        @!stack.push: $ctx;
        LEAVE @!stack.pop;
        &code()
    }

    method checkin {
        for @.stack.reverse -> $ctx {
            $ctx.checkin;
        }
    }
    method checkout {
        for @.stack.reverse -> $ctx {
            $ctx.checkout;
        }
    }
}

sub enter-ast-ctx(RakuAST::Node:D $node, ASTContext:D $ctx) {
    $*CTX-STACK.enter-ctx: $ctx, {
        traverse-ast($node)
    }
}

sub replace-paragraph($from, $to, :$modify = True) {
    my $parent = $*DOC-NODE-PARENT;
    my @para;
    for $parent.paragraphs -> $p {
        if $p === $from {
            @para.push: $to;
        }
        else {
            @para.push: $p;
        }
    }
    $parent.set-paragraphs(@para);
    $*DOC-CHANGED = $modify;

}

proto sub ast-literal($) {*}
multi sub ast-literal(Str:D $v) { RakuAST::StrLiteral.new($v) }
multi sub ast-literal(Int:D $v) { RakuAST::IntLiteral.new($v) }
multi sub ast-literal(Num:D $v) { RakuAST::NumLiteral.new($v) }
multi sub ast-literal(Rat:D $v) { RakuAST::RatLiteral.new($v) }

proto sub config-adverb($, $) {*}
multi sub config-adverb($key, Str:D $value) {
    $key => RakuAST::Circumfix::Parentheses.new(
            RakuAST::SemiList.new(
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::QuotedString.new(
                    segments   => (ast-literal($value),)))))
}
multi sub config-adverb($key, Bool:D $value) {
    $key => RakuAST::Term::Name.new( RakuAST::Name.from-identifier($value.Str) );
}
multi sub config-adverb($key, RakuAST::Node:D $value) {
    $key => RakuAST::Circumfix::Parentheses.new(
            RakuAST::SemiList.new(
                RakuAST::Statement::Expression.new(expression => $value)))
}
multi sub config-adverb($key, $value) {
    $key => RakuAST::Circumfix::Parentheses.new(
            RakuAST::SemiList.new(
                RakuAST::Statement::Expression.new(expression => ast-literal($value))))
}

sub example-text(IO:D $example) {
    my $in-example = False;
    my @example-lines;
    for $example.lines -> $line {
        if $line ~~ /^\h* '#?example' \h+ $<cmd>=[start | end] \h* $/ -> $m {
            $in-example = $m<cmd> eq 'start';
        }
        else {
            @example-lines.push: $line if $in-example;
        }
    }
    @example-lines.join("\n");
}

sub example-out(Str:D() $example) {
    my $p = run 'raku', '-I.', $example, :out;

    die "Example exited with rc=" ~ $p.exitcode ~ ", can't use it" unless $p.exitcode == 0;
    $p.out.slurp(:close)
}

sub para-text($txt) {
    $txt.trim-trailing ~ "\n"
}

sub make-example-paragraph(IO() $example) {
    die "Can't produce example paragraph for '" ~ $example ~ "': no such file"
        unless $example.e;

    my $ex-para;

    indir $BASE, {
        my $ex-relative = $example.relative;
        my %config =
            config-adverb("example", ~$ex-relative),
            config-adverb("mtime", $example.modified.ceiling);

        $ex-para = RakuAST::Doc::Block.new(:type<item>, :%config);

        $ex-para.add-paragraph("From ");
        $ex-para.add-paragraph:
            RakuAST::Doc::Markup.new(
                :letter<L>, :opener("<"), :closer(">"),
                atoms => ( ~$ex-relative ),
                meta => ( ~$example.relative($*DEST-FILE-DIR) )
            );
        $ex-para.add-paragraph: "\n\n";

        my $ex-flag = config-adverb("example", True);

        my $code = RakuAST::Doc::Block.new(
            :type<code>,
            config => %( config-adverb("lang", "raku"), $ex-flag ));
        $code.add-paragraph: para-text(example-text($example));
        $ex-para.add-paragraph($code);

        $ex-para.add-paragraph("Sample output:\n\n");

        my $out-block = RakuAST::Doc::Block.new( :type<output>, config => %( $ex-flag ) );
        $out-block.add-paragraph(example-out($example));
        $ex-para.add-paragraph($out-block);
    }

    $ex-para
}

sub update-example($ex-para, IO() $example) {
    for $ex-para.paragraphs.grep({ $_ ~~ RakuAST::Doc::Block && (.config andthen .<example> andthen .EVAL) }) -> $para {
        given $para.type {
            when 'code' {
                $para.set-paragraphs(para-text(example-text($example)));
            }
            when 'output' {
                $para.set-paragraphs(para-text(example-out($example)));
            }
        }
    }
}

proto sub ast-node(|) {*}

multi sub ast-node(RakuAST::Doc::Markup $markup where *.letter eq 'L') {
    return traverse-ast($markup) if $markup.meta;

    my $ctx = ASTLink.new;
    enter-ast-ctx $markup, $ctx;

    if $ctx.is-mod-link {
        my $m = $ctx.text-node.atoms.head ~~ &RxMod;

        $ctx.text-node.set-atoms($m<module> ~ $m<smile>);

        my $mod-name = ~$m<module>;
        my $link;

        if $mod-name ~~ /$MAIN-MOD <.wb>/ {
            # Until rakudoc: schema is supported by standard tools, stick to 'md' format.
            my $link-path = IO::Spec::Unix.catfile('docs', 'md', |$mod-name.split('::')) ~ '.md';
            $link = ~$link-path.IO.relative($*DEST-FILE-DIR);
        }
        else {
            $link = 'https://raku.land/';

            if $m<author> {
                $link ~= $m<author> ~ '/' ~ $m<module>;
                if $m<version> {
                    $link ~= '?v=' ~ $m<version>;
                }
            }
            else {
                $link ~= '?q=' ~ $m<module>
            }
        }

        $markup.set-meta($link);

        $*DOC-CHANGED = True;
    }
}

multi sub ast-node(RakuAST::Doc::Markup:D $m) {
    traverse-ast($m);
}

multi sub ast-node(RakuAST::Doc::Block:D $b where *.type eq 'item') {
    with $b.config<example> {
        indir $BASE, {
            my $example = $b.config<example>.EVAL.IO;
            my $mtime = $b.config<mtime>.EVAL;

            if $example.modified > $mtime {
                update-example($b, $example);
                $*DOC-CHANGED = True;
            }
        }
    }
    nextsame;
}

multi sub ast-node(RakuAST::Doc::Block:D $ex where *.type eq 'EXAMPLE') {
    my $spec := IO::Spec::Unix;
    my $example = ($ex.paragraphs.head andthen .trim);
    die "Example declaration is missing example file" unless $example;
    die "Example cannot be absolute, got '$example'" if $spec.is-absolute($example);
    $example = $spec.canonpath($example);
    my @components = $spec.splitdir($example);
    if @components == 1 {
        $example = $spec.catdir($BASE, "examples", $example);
    }
    else {
        $example = $spec.catdir($BASE, |@components);
    }

    unless $example.IO.e {
        $example ~= '.raku';
    }

    # tr-note "EXAMPLE FILE: ", $example;
    my $ex-para = make-example-paragraph($example);
    replace-paragraph($ex, $ex-para);
    $*DOC-CHANGED = True;
}

multi sub ast-node(RakuAST::Node:D $node) {
    traverse-ast($node)
}

proto sub traverse-ast(|) {*}

multi sub traverse-ast(Str:D $doc) {
    my $*DOC-CHANGED = False;
    my $*DOC-AST-LEVEL = 0;
    my $*CTX-STACK = ASTContextStack.new;
    my $ast = $doc.AST;
    traverse-ast $ast;

    return Nil unless $*DOC-CHANGED;

    # Workaround a bug in the DEPARSE implementation where extra newlines are added
    my role ItemDeparse {
        multi method deparse(RakuAST::Doc::Block:D $ast --> Str:D) {
            nextsame unless $ast.type eq 'item' | 'head';
            my $deparsed = callsame();
            CATCH {
                note "THROWN FOR ", $ast;
                note "   para: ", $ast.paragraphs[*-1];
            }
            my $last-line;
            given $ast.paragraphs[*-1] {
                when Str {
                    $last-line = $_;
                }
                when RakuAST::Doc::Paragraph {
                    $last-line = .atoms[*-1];
                }
                default {
                    return $deparsed;
                }
            }
            my $new-lines = .<nl>.Str given $last-line ~~ /[^^ || \N] $<nl>=\n*$/;
            return $deparsed.trim-trailing ~ $new-lines;
        }
    }
    $ast.DEPARSE(ItemDeparse);
}


multi sub traverse-ast(RakuAST::Node:D $node, &handler?) {
    # tr-note($node.^name) if $VERBOSE;
    my $level = $*DOC-AST-LEVEL + 1;
    my $*DOC-NODE-PARENT = $node;
    my @*DOC-NODE-CHILDREN;
    $*CTX-STACK.checkin;
    $node.visit-children(-> $child {
        my $*DOC-NODE = $child;
        my $*DOC-AST-LEVEL := $level;
        @*DOC-NODE-CHILDREN.push: $child;
        ast-node($child);
    });
    $*CTX-STACK.checkout;
}

sub patch-a-doc(Str:D $pod-file, :$output --> Str) {
    note "===> Updating ", $pod-file if $VERBOSE;
    my Bool $backup = False;
    my $src = $pod-file.IO.slurp;
    my $actions = MyPOD-Actions.new(
        :$pod-file,
        |(:dest-file-dir(.IO.parent(1)) with $output)
    );
    my $res = MyPOD.parse($src, :$actions);

    die "Failed to parse the source" unless $res;

    my $*DEST-FILE-DIR = $actions.dest-file-dir;
    my $changed = traverse-ast($res.made);

    if $FORCE || $actions.replaced || $changed {
        if $backup {
            my $idx = 0;
            my $bak-file = $pod-file ~ ".bk";
            while $bak-file.IO.e {
                $bak-file = $pod-file ~ (++$idx).fmt(".%02d.bk");
            }
            $pod-file.IO.rename($bak-file);
        }

        $pod-file.IO.spurt($changed || $res.made);
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
    my @opts = (%*ENV<RAKU_OPTS> andthen .split(" "));
    my @cmd = ~$*EXECUTABLE, '--doc=' ~ $fmt, |@opts, $src;
    note '$ ', @cmd.join(" "), ' >', ~$dst if $VERBOSE;
    run |@cmd, :out($dfh);
}

proto gen-fmt(Str:D, $, $, *%) {*}
multi gen-fmt('md', $src, $base, :$output) {
    my $md-dest = make-dest($src, $base, 'md', :$output);
    my $md-mtime = $md-dest.modified;
    if !$md-dest.e || $src.IO.modified > $md-mtime || $*PROGRAM.modified > $md-mtime {
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
        note "??? $pod-file" if $VERBOSE;
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
                    my $*DOC-OUT-FMT = $fmt;
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
