#!/usr/bin/env raku
use v6.e.PREVIEW;

sub MAIN(IO() $base-dir = ".") {
    my $lib-dir = $base-dir.add("lib");
    for qqx<git ls-files $lib-dir>.lines -> IO() $f {
        my $mod = $f.relative($base-dir).IO;
        my $name = $mod.basename.IO.extension("rakudoc", :1parts);
        my $dir = $base-dir.add(IO::Spec::Unix.catdir("doc", |(IO::Spec::Unix.splitdir($mod.dirname).[1..*]))).IO;
        unless $dir.d {
            say "+ directory ", ~$dir.relative;
            $dir.mkdir.sink;
        }
        my $doc = $dir.add($name).IO;
        my $modname = (|IO::Spec::Unix.splitdir($f.dirname.IO.relative($lib-dir)), $name.IO.extension("")).join("::");
        unless $doc.e {
            say "+ template for $modname";
            $doc.spurt: qq:to<EODOC>;
=begin pod
=head1 NAME

C<$modname> - from I<{$mod}>

=end pod
EODOC
        }
    }
}