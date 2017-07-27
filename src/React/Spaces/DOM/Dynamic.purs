module React.Spaces.DOM.Dynamic where

import Control.Monad.Free (Free)
import Prelude (Unit)
import React (ReactClass)
import React.DOM (IsDynamic(..))
import React.Spaces (SpaceF, rClsNode, rDOMNode, rEmptyNode, rTextNode)

cls :: forall p. ReactClass p -> p -> Free SpaceF Unit -> Free SpaceF Unit
cls = rClsNode

text :: String -> Free SpaceF Unit
text = rTextNode

empty :: Free SpaceF Unit
empty = rEmptyNode

a :: Free SpaceF Unit -> Free SpaceF Unit
a = rDOMNode "a" [] (IsDynamic true)

abbr :: Free SpaceF Unit -> Free SpaceF Unit
abbr = rDOMNode "abbr" [] (IsDynamic true)

address :: Free SpaceF Unit -> Free SpaceF Unit
address = rDOMNode "address" [] (IsDynamic true)

area :: Free SpaceF Unit -> Free SpaceF Unit
area = rDOMNode "area" [] (IsDynamic true)

article :: Free SpaceF Unit -> Free SpaceF Unit
article = rDOMNode "article" [] (IsDynamic true)

aside :: Free SpaceF Unit -> Free SpaceF Unit
aside = rDOMNode "aside" [] (IsDynamic true)

audio :: Free SpaceF Unit -> Free SpaceF Unit
audio = rDOMNode "audio" [] (IsDynamic true)

b :: Free SpaceF Unit -> Free SpaceF Unit
b = rDOMNode "b" [] (IsDynamic true)

base :: Free SpaceF Unit -> Free SpaceF Unit
base = rDOMNode "base" [] (IsDynamic true)

bdi :: Free SpaceF Unit -> Free SpaceF Unit
bdi = rDOMNode "bdi" [] (IsDynamic true)

bdo :: Free SpaceF Unit -> Free SpaceF Unit
bdo = rDOMNode "bdo" [] (IsDynamic true)

big :: Free SpaceF Unit -> Free SpaceF Unit
big = rDOMNode "big" [] (IsDynamic true)

blockquote :: Free SpaceF Unit -> Free SpaceF Unit
blockquote = rDOMNode "blockquote" [] (IsDynamic true)

body :: Free SpaceF Unit -> Free SpaceF Unit
body = rDOMNode "body" [] (IsDynamic true)

br :: Free SpaceF Unit -> Free SpaceF Unit
br = rDOMNode "br" [] (IsDynamic true)

button :: Free SpaceF Unit -> Free SpaceF Unit
button = rDOMNode "button" [] (IsDynamic true)

canvas :: Free SpaceF Unit -> Free SpaceF Unit
canvas = rDOMNode "canvas" [] (IsDynamic true)

caption :: Free SpaceF Unit -> Free SpaceF Unit
caption = rDOMNode "caption" [] (IsDynamic true)

cite :: Free SpaceF Unit -> Free SpaceF Unit
cite = rDOMNode "cite" [] (IsDynamic true)

code :: Free SpaceF Unit -> Free SpaceF Unit
code = rDOMNode "code" [] (IsDynamic true)

col :: Free SpaceF Unit -> Free SpaceF Unit
col = rDOMNode "col" [] (IsDynamic true)

colgroup :: Free SpaceF Unit -> Free SpaceF Unit
colgroup = rDOMNode "colgroup" [] (IsDynamic true)

_data :: Free SpaceF Unit -> Free SpaceF Unit
_data = rDOMNode "data" [] (IsDynamic true)

datalist :: Free SpaceF Unit -> Free SpaceF Unit
datalist = rDOMNode "datalist" [] (IsDynamic true)

dd :: Free SpaceF Unit -> Free SpaceF Unit
dd = rDOMNode "dd" [] (IsDynamic true)

del :: Free SpaceF Unit -> Free SpaceF Unit
del = rDOMNode "del" [] (IsDynamic true)

details :: Free SpaceF Unit -> Free SpaceF Unit
details = rDOMNode "details" [] (IsDynamic true)

dfn :: Free SpaceF Unit -> Free SpaceF Unit
dfn = rDOMNode "dfn" [] (IsDynamic true)

dialog :: Free SpaceF Unit -> Free SpaceF Unit
dialog = rDOMNode "dialog" [] (IsDynamic true)

div :: Free SpaceF Unit -> Free SpaceF Unit
div = rDOMNode "div" [] (IsDynamic true)

dl :: Free SpaceF Unit -> Free SpaceF Unit
dl = rDOMNode "dl" [] (IsDynamic true)

dt :: Free SpaceF Unit -> Free SpaceF Unit
dt = rDOMNode "dt" [] (IsDynamic true)

em :: Free SpaceF Unit -> Free SpaceF Unit
em = rDOMNode "em" [] (IsDynamic true)

embed :: Free SpaceF Unit -> Free SpaceF Unit
embed = rDOMNode "embed" [] (IsDynamic true)

fieldset :: Free SpaceF Unit -> Free SpaceF Unit
fieldset = rDOMNode "fieldset" [] (IsDynamic true)

figcaption :: Free SpaceF Unit -> Free SpaceF Unit
figcaption = rDOMNode "figcaption" [] (IsDynamic true)

figure :: Free SpaceF Unit -> Free SpaceF Unit
figure = rDOMNode "figure" [] (IsDynamic true)

footer :: Free SpaceF Unit -> Free SpaceF Unit
footer = rDOMNode "footer" [] (IsDynamic true)

form :: Free SpaceF Unit -> Free SpaceF Unit
form = rDOMNode "form" [] (IsDynamic true)

h1 :: Free SpaceF Unit -> Free SpaceF Unit
h1 = rDOMNode "h1" [] (IsDynamic true)

h2 :: Free SpaceF Unit -> Free SpaceF Unit
h2 = rDOMNode "h2" [] (IsDynamic true)

h3 :: Free SpaceF Unit -> Free SpaceF Unit
h3 = rDOMNode "h3" [] (IsDynamic true)

h4 :: Free SpaceF Unit -> Free SpaceF Unit
h4 = rDOMNode "h4" [] (IsDynamic true)

h5 :: Free SpaceF Unit -> Free SpaceF Unit
h5 = rDOMNode "h5" [] (IsDynamic true)

h6 :: Free SpaceF Unit -> Free SpaceF Unit
h6 = rDOMNode "h6" [] (IsDynamic true)

head :: Free SpaceF Unit -> Free SpaceF Unit
head = rDOMNode "head" [] (IsDynamic true)

header :: Free SpaceF Unit -> Free SpaceF Unit
header = rDOMNode "header" [] (IsDynamic true)

hr :: Free SpaceF Unit -> Free SpaceF Unit
hr = rDOMNode "hr" [] (IsDynamic true)

html :: Free SpaceF Unit -> Free SpaceF Unit
html = rDOMNode "html" [] (IsDynamic true)

i :: Free SpaceF Unit -> Free SpaceF Unit
i = rDOMNode "i" [] (IsDynamic true)

iframe :: Free SpaceF Unit -> Free SpaceF Unit
iframe = rDOMNode "iframe" [] (IsDynamic true)

img :: Free SpaceF Unit -> Free SpaceF Unit
img = rDOMNode "img" [] (IsDynamic true)

input :: Free SpaceF Unit -> Free SpaceF Unit
input = rDOMNode "input" [] (IsDynamic true)

ins :: Free SpaceF Unit -> Free SpaceF Unit
ins = rDOMNode "ins" [] (IsDynamic true)

kbd :: Free SpaceF Unit -> Free SpaceF Unit
kbd = rDOMNode "kbd" [] (IsDynamic true)

keygen :: Free SpaceF Unit -> Free SpaceF Unit
keygen = rDOMNode "keygen" [] (IsDynamic true)

label :: Free SpaceF Unit -> Free SpaceF Unit
label = rDOMNode "label" [] (IsDynamic true)

legend :: Free SpaceF Unit -> Free SpaceF Unit
legend = rDOMNode "legend" [] (IsDynamic true)

li :: Free SpaceF Unit -> Free SpaceF Unit
li = rDOMNode "li" [] (IsDynamic true)

link :: Free SpaceF Unit -> Free SpaceF Unit
link = rDOMNode "link" [] (IsDynamic true)

main :: Free SpaceF Unit -> Free SpaceF Unit
main = rDOMNode "main" [] (IsDynamic true)

map :: Free SpaceF Unit -> Free SpaceF Unit
map = rDOMNode "map" [] (IsDynamic true)

mark :: Free SpaceF Unit -> Free SpaceF Unit
mark = rDOMNode "mark" [] (IsDynamic true)

menu :: Free SpaceF Unit -> Free SpaceF Unit
menu = rDOMNode "menu" [] (IsDynamic true)

menuitem :: Free SpaceF Unit -> Free SpaceF Unit
menuitem = rDOMNode "menuitem" [] (IsDynamic true)

meta :: Free SpaceF Unit -> Free SpaceF Unit
meta = rDOMNode "meta" [] (IsDynamic true)

meter :: Free SpaceF Unit -> Free SpaceF Unit
meter = rDOMNode "meter" [] (IsDynamic true)

nav :: Free SpaceF Unit -> Free SpaceF Unit
nav = rDOMNode "nav" [] (IsDynamic true)

noscript :: Free SpaceF Unit -> Free SpaceF Unit
noscript = rDOMNode "noscript" [] (IsDynamic true)

object :: Free SpaceF Unit -> Free SpaceF Unit
object = rDOMNode "object" [] (IsDynamic true)

ol :: Free SpaceF Unit -> Free SpaceF Unit
ol = rDOMNode "ol" [] (IsDynamic true)

optgroup :: Free SpaceF Unit -> Free SpaceF Unit
optgroup = rDOMNode "optgroup" [] (IsDynamic true)

option :: Free SpaceF Unit -> Free SpaceF Unit
option = rDOMNode "option" [] (IsDynamic true)

output :: Free SpaceF Unit -> Free SpaceF Unit
output = rDOMNode "output" [] (IsDynamic true)

p :: Free SpaceF Unit -> Free SpaceF Unit
p = rDOMNode "p" [] (IsDynamic true)

param :: Free SpaceF Unit -> Free SpaceF Unit
param = rDOMNode "param" [] (IsDynamic true)

picture :: Free SpaceF Unit -> Free SpaceF Unit
picture = rDOMNode "picture" [] (IsDynamic true)

pre :: Free SpaceF Unit -> Free SpaceF Unit
pre = rDOMNode "pre" [] (IsDynamic true)

progress :: Free SpaceF Unit -> Free SpaceF Unit
progress = rDOMNode "progress" [] (IsDynamic true)

q :: Free SpaceF Unit -> Free SpaceF Unit
q = rDOMNode "q" [] (IsDynamic true)

rp :: Free SpaceF Unit -> Free SpaceF Unit
rp = rDOMNode "rp" [] (IsDynamic true)

rt :: Free SpaceF Unit -> Free SpaceF Unit
rt = rDOMNode "rt" [] (IsDynamic true)

ruby :: Free SpaceF Unit -> Free SpaceF Unit
ruby = rDOMNode "ruby" [] (IsDynamic true)

s :: Free SpaceF Unit -> Free SpaceF Unit
s = rDOMNode "s" [] (IsDynamic true)

samp :: Free SpaceF Unit -> Free SpaceF Unit
samp = rDOMNode "samp" [] (IsDynamic true)

script :: Free SpaceF Unit -> Free SpaceF Unit
script = rDOMNode "script" [] (IsDynamic true)

section :: Free SpaceF Unit -> Free SpaceF Unit
section = rDOMNode "section" [] (IsDynamic true)

select :: Free SpaceF Unit -> Free SpaceF Unit
select = rDOMNode "select" [] (IsDynamic true)

small :: Free SpaceF Unit -> Free SpaceF Unit
small = rDOMNode "small" [] (IsDynamic true)

source :: Free SpaceF Unit -> Free SpaceF Unit
source = rDOMNode "source" [] (IsDynamic true)

span :: Free SpaceF Unit -> Free SpaceF Unit
span = rDOMNode "span" [] (IsDynamic true)

strong :: Free SpaceF Unit -> Free SpaceF Unit
strong = rDOMNode "strong" [] (IsDynamic true)

style :: Free SpaceF Unit -> Free SpaceF Unit
style = rDOMNode "style" [] (IsDynamic true)

sub :: Free SpaceF Unit -> Free SpaceF Unit
sub = rDOMNode "sub" [] (IsDynamic true)

summary :: Free SpaceF Unit -> Free SpaceF Unit
summary = rDOMNode "summary" [] (IsDynamic true)

sup :: Free SpaceF Unit -> Free SpaceF Unit
sup = rDOMNode "sup" [] (IsDynamic true)

table :: Free SpaceF Unit -> Free SpaceF Unit
table = rDOMNode "table" [] (IsDynamic true)

tbody :: Free SpaceF Unit -> Free SpaceF Unit
tbody = rDOMNode "tbody" [] (IsDynamic true)

td :: Free SpaceF Unit -> Free SpaceF Unit
td = rDOMNode "td" [] (IsDynamic true)

textarea :: Free SpaceF Unit -> Free SpaceF Unit
textarea = rDOMNode "textarea" [] (IsDynamic true)

tfoot :: Free SpaceF Unit -> Free SpaceF Unit
tfoot = rDOMNode "tfoot" [] (IsDynamic true)

th :: Free SpaceF Unit -> Free SpaceF Unit
th = rDOMNode "th" [] (IsDynamic true)

thead :: Free SpaceF Unit -> Free SpaceF Unit
thead = rDOMNode "thead" [] (IsDynamic true)

time :: Free SpaceF Unit -> Free SpaceF Unit
time = rDOMNode "time" [] (IsDynamic true)

title :: Free SpaceF Unit -> Free SpaceF Unit
title = rDOMNode "title" [] (IsDynamic true)

tr :: Free SpaceF Unit -> Free SpaceF Unit
tr = rDOMNode "tr" [] (IsDynamic true)

track :: Free SpaceF Unit -> Free SpaceF Unit
track = rDOMNode "track" [] (IsDynamic true)

u :: Free SpaceF Unit -> Free SpaceF Unit
u = rDOMNode "u" [] (IsDynamic true)

ul :: Free SpaceF Unit -> Free SpaceF Unit
ul = rDOMNode "ul" [] (IsDynamic true)

var :: Free SpaceF Unit -> Free SpaceF Unit
var = rDOMNode "var" [] (IsDynamic true)

video :: Free SpaceF Unit -> Free SpaceF Unit
video = rDOMNode "video" [] (IsDynamic true)

wbr :: Free SpaceF Unit -> Free SpaceF Unit
wbr = rDOMNode "body" [] (IsDynamic true)
