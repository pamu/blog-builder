{-# LANGUAGE QuasiQuotes #-}

module Props where

import           Text.RawString.QQ

brandName :: String
brandName = "Haskworks"

cardColor :: String
cardColor = "light-blue darken-4"

textColor :: String
textColor = "white-text"

tagTextColor :: String
tagTextColor = "indigo-text"

font :: String
font = [r|
html {
  font-family: GillSans, Calibri, Trebuchet, sans-serif;
}
|]

codeHighlight :: String
codeHighlight = [r|

code > span.kw { color: skyblue; font-weight: bold; }
code > span.dt { color: GreenYellow; }
code > span.dv { color: DarkGoldenrod; }
code > span.bn { color: DarkGoldenrod; }
code > span.fl { color: DarkGoldenrod; }
code > span.ch { color: #4070a0; }
code > span.st { color: DarkOrange; }
code > span.co { color: #60a0b0; font-style: italic; }
code > span.ot { color: gold; }
code > span.al { color: #ff0000; font-weight: bold; }
code > span.fu { color: DarkGoldenrod;  }
code > span.er { color: #ff0000; font-weight: bold; }

.code {
  /*box-shadow: 3px 3px 5px 1px #888;*/
  border-radius: 10px;
/*  margin: 1em;*/
/*  padding: 1em;*/
/*  border: 1px solid #aaa;*/
    padding: 0.75em;

  font-size: 12pt;
  width: 45em;

  color: white;
  line-height: 1.2em;
  /* font-family: sans-serif; */
  font-family: 'Ubuntu mono', monospace;
  background: black;
  background-repeat: no-repeat;
  margin-top: 2em;
  margin-bottom: 3em;
 }

pre {
    /*box-shadow: 3px 3px 5px 1px #888;*/
    border-radius: 10px;
    /* margin: 1em; */
    /* padding: 1em; */
    /* border: 1px solid #aaa; */
    padding: 0.75em;
    font-size: 12pt;
    width: 48em;
    color: white;
    line-height: 1.2em;
    /* font-family: sans-serif; */
    font-family: 'Ubuntu mono', monospace;
    background: black;
    background-repeat: no-repeat;
    margin-top: 2em;
    margin-bottom: 3em;
    display: block;
}
|]

tagCSS :: String
tagCSS = [r|
.tag {
  font-size: 8px;
  background-color: white;
  border-radius: 1px;
  padding: 2px;
  margin: 4px;
}
|]

disqusPlugin :: String
disqusPlugin = [r|
<div id="disqus_thread"></div>
<script>

/**
*  RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
*  LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables*/
/*
var disqus_config = function () {
this.page.url = PAGE_URL;  // Replace PAGE_URL with your page's canonical URL variable
this.page.identifier = PAGE_IDENTIFIER; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
};
*/
(function() { // DON'T EDIT BELOW THIS LINE
var d = document, s = d.createElement('script');
s.src = 'https://haskworks.disqus.com/embed.js';
s.setAttribute('data-timestamp', +new Date());
(d.head || d.body).appendChild(s);
})();
</script>
<noscript>Please enable JavaScript to view the <a href="https://disqus.com/?ref_noscript">comments powered by Disqus.</a></noscript>
|]
