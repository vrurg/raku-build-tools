#!/usr/bin/env fish

pushd (path resolve (string join "/" (path dirname (status --current-filename)) ".."))

for mod in (find lib -name '*.rakumod')
    if grep -q '=begin \(pod\|rakudoc\)' $mod
        set docdir (string replace -r '^lib/' 'doc/' (path dirname $mod))
        set docname (path change-extension rakudoc (path basename $mod))
        set docpath (string join "/" $docdir $docname)
        set temp_file (string join "." $mod "temp")
        set bkmod (string join "." $mod "bk")

        mkdir -p $docdir
        echo $mod "=>" $docpath
        cp $mod $bkmod
        sed -n '/=begin pod/,/=end pod/p' $mod >$docpath
        sed '/=begin pod/,/=end pod/d' $mod >$temp_file
        cp $temp_file $mod
        rm $temp_file;
    end
end

popd
