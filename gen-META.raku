#!/usr/bin/env raku

use lib <lib>;
use META6;

sub MAIN(Str:D $module) {
    require ::($module);
    require ::($module ~ '::META');
    my %meta is Map = :version(::($module).^ver), |::('&' ~ $module ~ "::META::META6").();
    print META6.new(|%meta).to-json: :sorted-keys;
}