#!/usr/bin/env sh

echo '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
                           "http://www.w3.org/TR/html4/strict.dtd">
     <html>
       <head>
         <meta http-equiv="Content-Type" content="text/html; charset=utf-8" >
       </head>
     <body>' > priv/www/doc/api.html
markdown -x toc doc/api.md >> priv/www/doc/api.html
echo '</body>
      </html>' >> priv/www/doc/api.html

