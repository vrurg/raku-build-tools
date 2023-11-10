#!/usr/bin/env raku
use v6.e.PREVIEW;
use experimental :rakuast;
use Async::Workers;
use Cmark::Native;
use Cmark;
use Digest::SHA256::Native;
use JSON::Fast;
use LibXML::Document :HTML;
use LibXML::Enums;
use LibXML;
use OO::Monitors;
use Template::Mustache;
use URI;

my $VERBOSE = False;
my $MAIN-MOD = %*ENV<MAIN_MOD>;
my $META-MOD;
my $MODULE := Mu;
my IO::Path $BASE;
my IO::Path $DOC-DIR;
my IO::Path $DOCS-DIR;
my $MOD-VERSION;
my $MOD-AUTH;
my $FORCE = False;
my $URL;

my Lock:D $index-lock .= new;
my $INDEX-STATE;
my $INDEX-STATE-SUM = "";
my $INDEX-FILE;
my $INDEX-TEMPLATE;
my %DOC-IDX;

# Keys are link destination files, values are sets of documents where linked from.
my %LINK-DEST;
my $LINK-DEST-STATE;
my $DO-LINK-CHECK = False;
my $link-dest-lock = Lock.new;

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

    method version($/) {
        $.replaced ||= Version.new(~$/) ≠ $MOD-VERSION;
        make $!ver-str;
    }

    method !link-file-from-top-level($/, $file, Str $subdir = ".") {
        my $file-path = $BASE.IO.add( IO::Spec::Unix.catfile(|($_ with $subdir), ~$file) );
        # unless $file-path.e {
        #     note "!!! WARNING !!! Missing file '" ~ $file-path ~ "' in macro " ~ $*MACRO-KEYWORD;
        # }
        my $rel-path = $file-path.resolve.relative($BASE);
        my $url-path = 'file:' ~ $rel-path;
        $.replaced = True;
        my $lbr = $*MACRO-LBRACE || '«';
        my $rbr = $*MACRO-RBRACE || '»';
        make "L{$lbr}I{$lbr}" ~ $rel-path ~ "$rbr|" ~ $url-path ~ $rbr
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
        my $tdir = $BASE.add('t');
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
    $<module>=[ '..::'? [ <.alpha> <.alnum>* ] ** 1..* % '::' ]
    [
        [ ':auth<' $<auth>=[ .*? <?before '>'> ] '>' ]
        | [ ':ver<' $<ver>=[ .*? <?before '>'> ] '>' ]
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
    has $.is-mod-link = False;
    has $.is-file-link = False;
    has $.certainly-not = False;
    has $.text-node;

    method checkin {
        my $cur-node = $*DOC-NODE;
        $!is-mod-link &&= $cur-node ~~ RakuAST::Doc::Markup && $cur-node.atoms.elems < 2;
    }

    method checkout {
        return if $!certainly-not;

        # It can be a link to a module, but let's make sure first.
        my $it-can-be = True;

        my $cur-node = $*DOC-NODE;
        $!certainly-not ||=  $cur-node.atoms > 1;
        $it-can-be &&= $cur-node.atoms < 2;

        if $it-can-be  {
            if $cur-node.letter eq 'C' {
                if $cur-node.atoms.head ~~ &RxMod {
                    $!text-node = $cur-node;
                    $!is-mod-link = True;
                }
            }
            elsif $cur-node.letter eq 'I' {
                $!text-node = $cur-node;
                $!is-file-link = True;
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
    $*DOC-CHANGED = 'replace-paragraph' if $modify;

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
        my $ex-relative = $example.relative($BASE);
        my %config =
            config-adverb("example", ~$ex-relative),
            config-adverb("mtime", $example.modified.to-posix.head.ceiling);

        $ex-para = RakuAST::Doc::Block.new(:type<item>, :%config);

        $ex-para.add-paragraph("From ");
        $ex-para.add-paragraph:
            RakuAST::Doc::Markup.new(
                :letter<L>, :opener("«"), :closer("»"),
                atoms => ( ~$ex-relative ),
                meta => ( 'file:' ~ $ex-relative )
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

    my $ctx = ASTLink.new;
    enter-ast-ctx $markup, $ctx;

    my $meta = $markup.meta.head // "";

    unless $meta ~~ /^ \w+ ":"/ {
        # XXX Temporary, fix examples link
        # Skip if starts with scheme
        if $meta.contains('/examples/') {
            if $*FMT-REFERENCE-DIR.add($meta).e {
                my $link = 'file:' ~ $*FMT-REFERENCE-DIR.add($meta).resolve.relative($BASE);
                $markup.set-meta($link);
                $*DOC-CHANGED = 'example link';
            }
            else {
                note "!!! WARNING: no destination file found for " ~ $markup.DEPARSE, " in " ~ $*RAKUDOC-FILE, "\n",
                     "    EXPECTED to be found as " ~ $*FMT-REFERENCE-DIR.add($meta).resolve;
            }
        }
    }

    if $ctx.is-mod-link {
        my $link;

        my $uri;
        my $scheme = "";

        if $meta ~~ rx{^ rakudoc ":" "//"? } {
            $scheme = 'rakudoc';
        }
        else {
            $uri = URI.new($meta);
            $scheme = $uri.scheme;
        }

        given $scheme {
            when "" | "file" {
                $link = "rakudoc:" ~ $ctx.text-node.atoms.head;
            }
            when 'http' | 'https' {
                if $uri.host eq 'raku.land' {
                    if $uri.path.segments > 2 {
                        my ($sidx, $auth) = $uri.path.segments.first(*.chars, :kv);
                        my $ver = $uri.query-form<v>.head;
                        my $mod = ($uri.path andthen .segments[$sidx + 1]);
                        $link = 'rakudoc:' ~ $mod ~ ':auth<' ~ $auth ~ ">" ~ ($ver ?? ":ver<$ver>" !! "");
                    }
                    elsif $uri.query-form<q> -> $mod {
                        $link = 'rakudoc:' ~ $mod;
                    }
                }
            }
        }

        return without $link;

        $markup.set-meta($link);
        if $link.contains('>') {
            $markup.set-opener('«');
            $markup.set-closer('»');
        }
        $*DOC-CHANGED = 'module link';
    }
    elsif $ctx.is-file-link {

        my $link;
        my $link-text;
        my $uri = URI.new($meta);
        # If host is specified it's not our business to mangle with this link.
        return if $uri.host;

        my $project-path;
        if $uri.path.segments.head eq '..' | '.' {
            my $rel-path = $*FMT-REFERENCE-DIR.add(~$uri.path).IO.resolve.relative($BASE);
            $link = 'file:' ~ $rel-path;
            $link-text = ~$rel-path;
        }

        return without $link;

        $ctx.text-node.set-atoms($link-text);
        $markup.set-meta($link);
        $*DOC-CHANGED = 'file link';
    }
    else {
        nextsame
    }
}

multi sub ast-node(RakuAST::Doc::Markup:D $m) {
    traverse-ast($m);
}

multi sub ast-node(RakuAST::Doc::Block:D $b where *.type eq 'item') {
    with $b.config<example> {
        indir $BASE, {
            my $example = $b.config<example>.EVAL.IO;
            my $mtime = Instant.from-posix($b.config<mtime>.EVAL);

            if $example.modified > $mtime {
                update-example($b, $example);
                $b.set-config: %( |$b.config, config-adverb('mtime', $example.modified.to-posix.head.ceiling));
                $*DOC-CHANGED = 'example updated';
            }
        }
    }
    nextsame;
}

multi sub ast-node(RakuAST::Doc::Block:D $cfg where *.type eq 'config') {
    with $cfg.config<index> {
        my $index = $*DOC-IN-IDX := .EVAL;
        unless $index {
            my $doc-ref = make-doc-reference($*DOC-OUTPUT // $*RAKUDOC-FILE);
            update-doc-index($doc-ref, :$index);
        }
    }
    with $cfg.config<title> {
        $*DOC-TITLE = .EVAL;
    }
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

    my $ex-para = make-example-paragraph($example);
    replace-paragraph($ex, $ex-para, :modify);
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

    note "Doc changed reason: ", $*DOC-CHANGED if $VERBOSE;

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
            return $deparsed.trim-trailing ~ ($ast.abbreviated ?? $new-lines !! "\n\n");
        }
    }
    $ast.DEPARSE(ItemDeparse);
}


multi sub traverse-ast(RakuAST::Node:D $node, &handler?) {
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

sub patch-a-doc(Str:D $pod-file --> Str) {
    note "===> Updating ", $pod-file if $VERBOSE;
    my Bool $backup = False;
    my $src = $pod-file.IO.slurp;
    my $actions = MyPOD-Actions.new( :$pod-file );
    my $res = MyPOD.parse($src, :$actions);

    die "Failed to parse the source" unless $res;

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
sub make-dest($src is copy, :$base is copy = $BASE, :$fmt = $*DOC-OUT-FMT, :$ext = $*DOC-OUT-EXT, :$output) {
    $mdl.protect: {
        if $VERBOSE {
            $src = $src.absolute;
            $base = $base.absolute;
            my @d = $*SPEC.splitdir($src.IO.relative($base))[1 ..*];
            note "___ from $src base:$base ---> ", ~$src.IO.relative($base);
            note ">>>> [$ext] ", @d.join(", "), " ---> ", my $res = $*SPEC.catdir($DOCS-DIR, $ext,
                |$*SPEC.splitdir($src.IO.relative($base))[1 ..*]);
            note "!!! $src --- ", $res.IO.extension($ext, :0parts);
        }
        ($output
            ?? $output.IO.extension($ext, :0parts)
            !! $*SPEC.catdir( $DOCS-DIR,
                              $fmt,
                              |$*SPEC.splitdir($src.IO.relative($base))[1 ..*]) ).IO.extension($ext);
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

sub read-missing-links {
    if $LINK-DEST-STATE.open(:r, :shared) -> IO::Handle:D $fh {
        LEAVE $fh.close;
        if $fh.lock(:shared) {
            LEAVE $fh.unlock;
            if $fh.slurp.trim -> $link-json {
                %LINK-DEST = from-json($link-json);
                with %LINK-DEST<rakudoc> -> %rakudocs {
                    for %rakudocs.values -> %abs-rakudoc {
                        %abs-rakudoc{$_} .= SetHash for %abs-rakudoc.keys;
                    }
                }
            }
        }
        else -> $failure {
            $failure.exception.rethrow
        }
    }
}

sub save-missing-links {
    my $fh = $LINK-DEST-STATE.open(:w, :!shared);
    LEAVE $fh.close;
    if $fh.lock(:!shared) {
        LEAVE $fh.unlock;
        $fh.spurt: %LINK-DEST ?? to-json(%LINK-DEST) !! '';
    }
    else -> $failure {
        $failure.exception.rethrow
    }
}

sub register-link-dest(Str:D $scheme, IO:D() $link-path, Str:D() $for, Bool :$missing) {
    $link-dest-lock.protect: {
        my $abs-dest = $link-path.absolute($BASE);
        my $abs-rakudoc = $*RAKUDOC-FILE.IO.absolute($BASE);
        if $missing {
            %LINK-DEST<missing>{$abs-dest} //= %(:$scheme, :$for);
        }
        (%LINK-DEST<rakudoc>{$abs-rakudoc}{$*DOC-OUT-FMT} //= SetHash.new).set: $abs-dest;
    }
}

sub cleanup-link-dest {
    with $*RAKUDOC-FILE {
        my $abs-rakudoc = .IO.absolute($BASE);
        with $*DOC-OUT-FMT {
            %LINK-DEST<rakudoc>{$abs-rakudoc}.DELETE-KEY($_);
        }
    }
}

sub check-missing-links {
    $link-dest-lock.protect: {
        say "===> Checking for missing link destinations";
        # Delete references to no more existing rakudocs
        my %registered-dest;

        for %LINK-DEST<rakudoc>.pairs -> (:key($abs-rakudoc), :value(%fmts)) {
            if $abs-rakudoc.IO.e {
                for %fmts.values -> $destinations {
                    (%registered-dest{$_} //= SetHash.new).set($abs-rakudoc) for $destinations.keys;
                }
            }
            else {
                %LINK-DEST.DELETE-KEY($abs-rakudoc);
            }
        }

        for %LINK-DEST<missing>.sort(*.key) -> (:key($link-path), :value(%details)) {
            if %registered-dest.EXISTS-KEY($link-path) && !$link-path.IO.e {
                note "!!! WARNING !!! Missing target file for for local link '", %details<scheme>,
                     ":", %details<for>, "':\n",
                     "                destination: ", $link-path.IO.relative($BASE), "\n",
                     "                   found in: ",
                    %registered-dest{$link-path}
                        .keys.sort.kv
                        .map(-> $idx, $in { $in.IO.relative($BASE).indent($idx ?? 29 !! 0) })
                        .join("\n");
            }
            else {
                %LINK-DEST<missing>.DELETE-KEY($link-path);
            }
        }
    }
}

proto sub translate-uri(Str:D, Str:D, |) {*}

multi sub translate-uri('rakudoc', $mod, :$ext = $*DOC-OUT-EXT) {
    nextsame unless my $m = $mod ~~ &RxMod;

    my $auth = $m<auth>;
    my $version = $m<ver>;
    my $module = $m<module>;
    my $fmt = $*DOC-OUT-FMT;

    if $module.starts-with("..::")
       || ($module ~~ /^ $MAIN-MOD <.wb> / && (!$auth || $auth eq $MOD-AUTH))
    {
        # Our local module.
        my $link-path =
            (%DOC-IDX{$module} andthen .{$fmt})
            // IO::Spec::Unix.catfile($DOCS-DIR, $fmt, |$module.split('::')) ~ "." ~ $ext;
        my $link-dest = $link-path.IO.relative($*FMT-OUT-DIR);
        if $link-path.IO.e {
            register-link-dest('rakudoc', $link-path, $module);
        }
        else {
            register-link-dest('rakudoc', $link-path, $module, :missing);
        }
        return $link-dest;
    }

    my $link = 'https://raku.land/';

    if $auth {
        $link ~= $auth ~ '/' ~ $module;
        if $version {
            $link ~= '?v=' ~ $version;
        }
    }
    else {
        $link ~= '?q=' ~ $module
    }
    $link
}

multi sub translate-uri('file', $path) {
    my $dest = $BASE.add($path);
    unless $dest.e {
        register-link-dest('file', $dest, $path, :missing);
    }
    $dest.relative($*FMT-OUT-DIR)
}

multi sub translate-uri(Str:D, Str:D, |) { Nil }

proto sub fixup-fmt(Str:D, |) {*}

multi sub fixup-fmt('md', IO() $output) {
    my $*FMT-OUT-DIR = $output.parent(1);
    my $modified = False;

    my sub traverse-cnode($cnode) {
        my $next = $cnode;
        while $next {
            if $next.type eq 'link' {
                my $url = cmark_node_get_url($next);
                if $url ~~ rx{^ $<scheme>=\w+ ":" $<reference>=.+ $ } -> $m {
                    my $scheme = ~$m<scheme>;
                    with translate-uri($scheme, ~$m<reference>) {
                        cmark_node_set_url($next, $_);
                        $modified = True;
                    }
                }
            }

            traverse-cnode($_) with $next.first-child;

            $next = $next.next;
        }
    }

    my $cmark = Cmark.parse: $output.slurp(:close);

    traverse-cnode($cmark.node);
    if $modified {
        $output.spurt: $cmark.to-commonmark;
    }
}

multi sub fixup-fmt('html', IO() $output) {
    my $*FMT-OUT-DIR = $output.parent(1);
    my $modified = False;

    my $config = LibXML::Config.new: :quiet, :with-cache, :max-errors(1_000_000);
    my $parser = LibXML::Parser.new: :$config, :html;
    my HTML $html = $parser.parse: $output.slurp(:close), :html, :huge, :recover(2), :recover-silently, :dtd;

    for $html.findnodes(q«//a[starts-with(@href, "rakudoc:") or starts-with(@href, "file:")]») -> $node {
        my ($scheme, $reference) = $node.getAttribute('href').split(":", 2);
        with translate-uri($scheme, $reference) {
            $node.setAttribute('href', $_);
            $modified = True;
        }
    }


    if $modified {
        with $html.findnodes(q«//head/style»).head {
            my $style-patch = .cloneNode;
            $style-patch.appendChild: LibXML::CDATA.new(:content(q:to/CSS-PATCH/));
                .toc-level-1 .toc-text { padding-left: .5em; }
                .toc-level-2 .toc-text { padding-left: 1.5em; }
                .toc-level-3 .toc-text { padding-left: 2.5em; }
                .toc-level-4 .toc-text { padding-left: 3.5em; }
                .toc-level-5 .toc-text { padding-left: 4.5em; }
                #TOC * { border-width: 0; }
                CSS-PATCH
            .parent.appendChild: $style-patch;
        }
        $html.findnodes(q«//head/title»).head.appendText($*DOC-TITLE);
        with $html.findnodes(q«//body//h1[contains(., "rakudoc")]»).head {
            .unbindNode;
        }
        $output.spurt: $html.Str(:options(XML_SAVE_AS_HTML));
    }
}

sub dest-outdated(IO() $dest, +@src) {
    return True unless $dest.e;
    my $dest-mtime = $dest.modified;
    ? ($*PROGRAM, |@src).first:
        -> IO() $src {
            $src.modified > $dest-mtime
        }
}

proto gen-fmt(Str:D, $, *%) {*}
multi gen-fmt('md', $src, :$output) {
    my $*DOC-OUT-EXT = 'md';
    my $md-dest = make-dest($src, :$output);
    if dest-outdated($md-dest, $src) {
        say "===> Generating ", ~$md-dest.relative($BASE);
        cleanup-link-dest;
        invoke-raku $src, $md-dest, 'Markdown';
        fixup-fmt('md', $md-dest);
    }
    $md-dest
}
multi gen-fmt('html', $src, :$output) {
    my $*DOC-OUT-EXT = 'html';
    my $html-dest = make-dest($src, :$output);
    if dest-outdated($html-dest, $src) {
        say "===> Generating ", ~$html-dest.relative($BASE);
        cleanup-link-dest;
        invoke-raku $src, $html-dest, 'HTML';
        fixup-fmt('html', $html-dest);
    }
    $html-dest
}

sub prepare-module {
    require ::($MAIN-MOD);
    my %meta;
    my $meta_mod = ($META-MOD, $MAIN-MOD, $MAIN-MOD ~ '::META').first: -> $mmod {
        next without $mmod;
        (try { require ::($mmod); True })
            && (try { %meta = ::('&' ~ $mmod ~ '::META6').() })
    };
    die "Can't fetch META6 hash, consider setting META_MOD environment variable or use --meta-mod" unless $meta_mod;
    $MODULE := ::($MAIN-MOD);
    $MOD-VERSION = $MODULE.^ver // %meta<ver>;
    $MOD-AUTH = $MODULE.^auth // %meta<auth>;
    $URL = S/\.git$// with %meta<source-url>;
}

sub update-doc-index(Str:D $reference, *%keys) {
    $index-lock.protect: {
        %DOC-IDX{$reference}{ .key } = .value for %keys;
    }
}

sub save-doc-index() {
    $index-lock.protect: {
        my $state-json = to-json %DOC-IDX, :pretty, :sorted-keys;

        # Do nothing if nothing has changed.
        unless sha256-hex($state-json) eq $INDEX-STATE-SUM {
            say "===> Index needs updating";
            my IO::Handle:D $idxh = $INDEX-STATE.open(:w, :!shared);
            LEAVE $idxh.close;
            if $idxh.lock(:!shared) {
                LEAVE $idxh.unlock;

                $idxh.spurt: $state-json;
            }
            else -> $failure {
                $failure.exception.rethrow
            }
        }
    }
}

sub read-doc-index {
    $index-lock.protect: {
        if $INDEX-STATE.open(:r, :shared) -> IO::Handle:D $idxh {
            LEAVE $idxh.close;
            if $idxh.lock(:shared) {
                LEAVE $idxh.unlock;

                my $idx-json = $idxh.slurp;

                $INDEX-STATE-SUM = sha256-hex $idx-json;

                if $idx-json {
                    %DOC-IDX = from-json $idx-json;
                }
            }
            else -> $failure {
                $failure.exception.rethrow
            }
        }
    }
}

sub refresh-index {
    if %DOC-IDX {
        if dest-outdated($INDEX-FILE, $INDEX-STATE, $INDEX-TEMPLATE) {
            if $INDEX-FILE.open(:w) -> $doch {
                my %tpl-data =
                    'main-mod' => $MAIN-MOD,
                    rakudoc =>
                        %DOC-IDX
                            .grep({ .value<index> // True })
                            .sort(*.value)
                            .map({ %( doc-reference => .key, doc-title => .value<doc-title> ) });

                my $mustache-logger = Template::Mustache::Logger.new: :level('Warn');
                for <Warn Error Fatal> -> $level {
                    $mustache-logger.routines{$level} = -> $lvl, $ex {
                        note "!!! Template $level: ", $ex.message;
                    };
                }
                $doch.spurt:
                    Template::Mustache.render($INDEX-TEMPLATE, %tpl-data);
            }
            else -> $failure {
                $failure.exception.rethrow
            }
        }
    }
}

sub make-doc-reference(IO() $doc-file) {
    my $parts = $doc-file.extension eq 'rakudoc' ?? 1 !! 0;
    $*SPEC.splitdir( $doc-file.relative($DOC-DIR).IO.extension("", :$parts) ).join("::");
}

sub gen-doc(+@pod-files, :$module, :$output, :$title, :%into --> Nil) {
    $MAIN-MOD = $_ with $module;
    prepare-module;
    my $wm = Async::Workers.new(:max-workers($*KERNEL.cpu-cores));
    for @pod-files -> $pod-file {
        note "??? $pod-file" if $VERBOSE;
        $wm.do-async: {
            my $*RAKUDOC-FILE = $pod-file;
            my $*DOC-OUTPUT := $output;
            my $md-sample = make-dest($pod-file, :fmt<md>, :ext<md>).parent(1).resolve;
            my $*FMT-REFERENCE-DIR = $md-sample.parent(1).resolve;
            my $reference = make-doc-reference($output || $pod-file);

            my $*DOC-TITLE;
            $index-lock.protect: {
                %DOC-IDX{$reference}<index> //= True;
                $*DOC-TITLE //= (%DOC-IDX{$reference} andthen .<title> ) // $reference;
            }

            my $*DOC-IN-IDX = True;
            patch-a-doc($pod-file);

            my $doc-title = $title // $*DOC-TITLE;
            my $doc-in-idx = $*DOC-IN-IDX;

            if $*DOC-IN-IDX {
                update-doc-index($reference, :$doc-title);
            }

            for %into.keys -> $fmt {
                next unless %into{$fmt};
                $wm.do-async: {
                    my $*RAKUDOC-FILE = $pod-file;
                    my $*DOC-OUTPUT := $output;
                    my $*DOC-OUT-FMT = $fmt;
                    my $*DOC-TITLE = $doc-title; # Retranslate the dynamic in this context.
                    my $*DOC-IN-IDX := $doc-in-idx;
                    update-doc-index $reference, |($fmt => gen-fmt($fmt, $pod-file, :$output).IO.relative($BASE));
                }
            }
        }
    }
    await $wm;
    $wm.shutdown;

    save-doc-index;
}

multi sub MAIN( +@rakudoc-file,
                Str :m(:$module)!,
                :$meta-mod = %*ENV<META_MOD>,
                IO::Path:D(Str:D) :$base = $*CWD,
                Bool :v(:$verbose),
                Bool :f(:$force) = False,
                Bool :$md = False,
                Bool :$html = False,
                #| Generate index file
                Bool :$index = False,
                #| Defaults to $BASE_DIR/doc/INDEX.rakudoc
                IO(Str) :$index-file,
                #| Defaults to $BASE_DIR/.gen-doc-index.json
                IO(Str) :$index-state,
                #| Defaults to $BUILD_TOOLS_DIR/gen-doc-index.mustache
                IO(Str) :$index-template,
                #| Defaults to $BASE_DIR/INDEX
                Str :$index-output,
                #| Make sure all local link destinations exists
                Bool :$check-links,
                #| Default title, only works for a single input rakudoc
                Str :$title,
                #| Where to send the output; only works for a single input rakudoc
                Str :o(:$output) )
{
    with ($output // $title) {
        if @rakudoc-file != 1 && (@rakudoc-file == 0 && $index) {
            die "Exactly one RakuDoc input file is required with any of --output, or --title";
        }
    }

    $VERBOSE = $_ with $verbose;
    $FORCE = $force;
    $META-MOD = $_ with $meta-mod;
    $BASE = $base;
    $DOC-DIR = $BASE.add: %*ENV<DOC_DIR> // 'doc';
    $DOCS-DIR = $BASE.add: %*ENV<DOCS_DIR> // 'docs';
    $INDEX-STATE = $index-state // $BASE.add('.gen-doc-index.json');
    $INDEX-FILE = $index-file // $DOC-DIR.add('INDEX.rakudoc');
    $INDEX-TEMPLATE = $index-template // $*PROGRAM.parent(1).add('gen-doc-index.mustache');
    $LINK-DEST-STATE = $BASE.add('.gen-doc-link-dest.json');

    read-doc-index;
    read-missing-links;

    my @psorted = @rakudoc-file.race.map({ [$_, .IO.s] }).sort({ @^b[1] cmp @^a[1] }).map({ .[0] });
    gen-doc(@psorted, :$module, :into{ :$md, :$html }, :$output, :$title);

    if $index {
        refresh-index;
        # Use --output for INDEX unless there is an input .rakudoc
        gen-doc(~$INDEX-FILE, :$module, :into{ :$md, :$html }, :output($index-output // $BASE.add('INDEX')));
    }

    check-missing-links if $check-links;
    save-missing-links;
}