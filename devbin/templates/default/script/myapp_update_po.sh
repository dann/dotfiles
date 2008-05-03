#!/bin/sh
(find root/templates/ -name '*.tt2' ; find lib/ -name '*.pm' )| xargs xgettext.pl -o lib/MyApp/I18N/ja.po
(find root/templates/ -name '*.tt2' ; find lib/ -name '*.pm' )| xargs xgettext.pl -o lib/MyApp/I18N/en.po
