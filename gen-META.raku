#!/usr/bin/env raku

use lib <lib>;
use META6;

sub MAIN(Str:D $module) {
    require ::($module);
	my $meta_mod = %*ENV<META_MOD> // $module ~ '::META';
    require ::($meta_mod);
    my %meta is Map = :version(::($module).^ver), |::('&' ~ $meta_mod ~ "::META6").();
    print META6.new(|%meta).to-json: :sorted-keys;
}
