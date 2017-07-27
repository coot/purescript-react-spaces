module React.Spaces.DOM where

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
a = rDOMNode "a" [] (IsDynamic false)

abbr :: Free SpaceF Unit -> Free SpaceF Unit
abbr = rDOMNode "abbr" [] (IsDynamic false)

address :: Free SpaceF Unit -> Free SpaceF Unit
address = rDOMNode "address" [] (IsDynamic false)

area :: Free SpaceF Unit -> Free SpaceF Unit
area = rDOMNode "area" [] (IsDynamic false)

article :: Free SpaceF Unit -> Free SpaceF Unit
article = rDOMNode "article" [] (IsDynamic false)

aside :: Free SpaceF Unit -> Free SpaceF Unit
aside = rDOMNode "aside" [] (IsDynamic false)

audio :: Free SpaceF Unit -> Free SpaceF Unit
audio = rDOMNode "audio" [] (IsDynamic false)

b :: Free SpaceF Unit -> Free SpaceF Unit
b = rDOMNode "b" [] (IsDynamic false)

base :: Free SpaceF Unit -> Free SpaceF Unit
base = rDOMNode "base" [] (IsDynamic false)

bdi :: Free SpaceF Unit -> Free SpaceF Unit
bdi = rDOMNode "bdi" [] (IsDynamic false)

bdo :: Free SpaceF Unit -> Free SpaceF Unit
bdo = rDOMNode "bdo" [] (IsDynamic false)

big :: Free SpaceF Unit -> Free SpaceF Unit
big = rDOMNode "big" [] (IsDynamic false)

blockquote :: Free SpaceF Unit -> Free SpaceF Unit
blockquote = rDOMNode "blockquote" [] (IsDynamic false)

body :: Free SpaceF Unit -> Free SpaceF Unit
body = rDOMNode "body" [] (IsDynamic false)

br :: Free SpaceF Unit -> Free SpaceF Unit
br = rDOMNode "br" [] (IsDynamic false)

button :: Free SpaceF Unit -> Free SpaceF Unit
button = rDOMNode "button" [] (IsDynamic false)

canvas :: Free SpaceF Unit -> Free SpaceF Unit
canvas = rDOMNode "canvas" [] (IsDynamic false)

caption :: Free SpaceF Unit -> Free SpaceF Unit
caption = rDOMNode "caption" [] (IsDynamic false)

cite :: Free SpaceF Unit -> Free SpaceF Unit
cite = rDOMNode "cite" [] (IsDynamic false)

code :: Free SpaceF Unit -> Free SpaceF Unit
code = rDOMNode "code" [] (IsDynamic false)

col :: Free SpaceF Unit -> Free SpaceF Unit
col = rDOMNode "col" [] (IsDynamic false)

colgroup :: Free SpaceF Unit -> Free SpaceF Unit
colgroup = rDOMNode "colgroup" [] (IsDynamic false)

_data :: Free SpaceF Unit -> Free SpaceF Unit
_data = rDOMNode "data" [] (IsDynamic false)

datalist :: Free SpaceF Unit -> Free SpaceF Unit
datalist = rDOMNode "datalist" [] (IsDynamic false)

dd :: Free SpaceF Unit -> Free SpaceF Unit
dd = rDOMNode "dd" [] (IsDynamic false)

del :: Free SpaceF Unit -> Free SpaceF Unit
del = rDOMNode "del" [] (IsDynamic false)

details :: Free SpaceF Unit -> Free SpaceF Unit
details = rDOMNode "details" [] (IsDynamic false)

dfn :: Free SpaceF Unit -> Free SpaceF Unit
dfn = rDOMNode "dfn" [] (IsDynamic false)

dialog :: Free SpaceF Unit -> Free SpaceF Unit
dialog = rDOMNode "dialog" [] (IsDynamic false)

div :: Free SpaceF Unit -> Free SpaceF Unit
div = rDOMNode "div" [] (IsDynamic false)

dl :: Free SpaceF Unit -> Free SpaceF Unit
dl = rDOMNode "dl" [] (IsDynamic false)

dt :: Free SpaceF Unit -> Free SpaceF Unit
dt = rDOMNode "dt" [] (IsDynamic false)

em :: Free SpaceF Unit -> Free SpaceF Unit
em = rDOMNode "em" [] (IsDynamic false)

embed :: Free SpaceF Unit -> Free SpaceF Unit
embed = rDOMNode "embed" [] (IsDynamic false)

fieldset :: Free SpaceF Unit -> Free SpaceF Unit
fieldset = rDOMNode "fieldset" [] (IsDynamic false)

figcaption :: Free SpaceF Unit -> Free SpaceF Unit
figcaption = rDOMNode "figcaption" [] (IsDynamic false)

figure :: Free SpaceF Unit -> Free SpaceF Unit
figure = rDOMNode "figure" [] (IsDynamic false)

footer :: Free SpaceF Unit -> Free SpaceF Unit
footer = rDOMNode "footer" [] (IsDynamic false)

form :: Free SpaceF Unit -> Free SpaceF Unit
form = rDOMNode "form" [] (IsDynamic false)

h1 :: Free SpaceF Unit -> Free SpaceF Unit
h1 = rDOMNode "h1" [] (IsDynamic false)

h2 :: Free SpaceF Unit -> Free SpaceF Unit
h2 = rDOMNode "h2" [] (IsDynamic false)

h3 :: Free SpaceF Unit -> Free SpaceF Unit
h3 = rDOMNode "h3" [] (IsDynamic false)

h4 :: Free SpaceF Unit -> Free SpaceF Unit
h4 = rDOMNode "h4" [] (IsDynamic false)

h5 :: Free SpaceF Unit -> Free SpaceF Unit
h5 = rDOMNode "h5" [] (IsDynamic false)

h6 :: Free SpaceF Unit -> Free SpaceF Unit
h6 = rDOMNode "h6" [] (IsDynamic false)

head :: Free SpaceF Unit -> Free SpaceF Unit
head = rDOMNode "head" [] (IsDynamic false)

header :: Free SpaceF Unit -> Free SpaceF Unit
header = rDOMNode "header" [] (IsDynamic false)

hr :: Free SpaceF Unit -> Free SpaceF Unit
hr = rDOMNode "hr" [] (IsDynamic false)

html :: Free SpaceF Unit -> Free SpaceF Unit
html = rDOMNode "html" [] (IsDynamic false)

i :: Free SpaceF Unit -> Free SpaceF Unit
i = rDOMNode "i" [] (IsDynamic false)

iframe :: Free SpaceF Unit -> Free SpaceF Unit
iframe = rDOMNode "iframe" [] (IsDynamic false)

img :: Free SpaceF Unit -> Free SpaceF Unit
img = rDOMNode "img" [] (IsDynamic false)

input :: Free SpaceF Unit -> Free SpaceF Unit
input = rDOMNode "input" [] (IsDynamic false)

ins :: Free SpaceF Unit -> Free SpaceF Unit
ins = rDOMNode "ins" [] (IsDynamic false)

kbd :: Free SpaceF Unit -> Free SpaceF Unit
kbd = rDOMNode "kbd" [] (IsDynamic false)

keygen :: Free SpaceF Unit -> Free SpaceF Unit
keygen = rDOMNode "keygen" [] (IsDynamic false)

label :: Free SpaceF Unit -> Free SpaceF Unit
label = rDOMNode "label" [] (IsDynamic false)

legend :: Free SpaceF Unit -> Free SpaceF Unit
legend = rDOMNode "legend" [] (IsDynamic false)

li :: Free SpaceF Unit -> Free SpaceF Unit
li = rDOMNode "li" [] (IsDynamic false)

link :: Free SpaceF Unit -> Free SpaceF Unit
link = rDOMNode "link" [] (IsDynamic false)

main :: Free SpaceF Unit -> Free SpaceF Unit
main = rDOMNode "main" [] (IsDynamic false)

map :: Free SpaceF Unit -> Free SpaceF Unit
map = rDOMNode "map" [] (IsDynamic false)

mark :: Free SpaceF Unit -> Free SpaceF Unit
mark = rDOMNode "mark" [] (IsDynamic false)

menu :: Free SpaceF Unit -> Free SpaceF Unit
menu = rDOMNode "menu" [] (IsDynamic false)

menuitem :: Free SpaceF Unit -> Free SpaceF Unit
menuitem = rDOMNode "menuitem" [] (IsDynamic false)

meta :: Free SpaceF Unit -> Free SpaceF Unit
meta = rDOMNode "meta" [] (IsDynamic false)

meter :: Free SpaceF Unit -> Free SpaceF Unit
meter = rDOMNode "meter" [] (IsDynamic false)

nav :: Free SpaceF Unit -> Free SpaceF Unit
nav = rDOMNode "nav" [] (IsDynamic false)

noscript :: Free SpaceF Unit -> Free SpaceF Unit
noscript = rDOMNode "noscript" [] (IsDynamic false)

object :: Free SpaceF Unit -> Free SpaceF Unit
object = rDOMNode "object" [] (IsDynamic false)

ol :: Free SpaceF Unit -> Free SpaceF Unit
ol = rDOMNode "ol" [] (IsDynamic false)

optgroup :: Free SpaceF Unit -> Free SpaceF Unit
optgroup = rDOMNode "optgroup" [] (IsDynamic false)

option :: Free SpaceF Unit -> Free SpaceF Unit
option = rDOMNode "option" [] (IsDynamic false)

output :: Free SpaceF Unit -> Free SpaceF Unit
output = rDOMNode "output" [] (IsDynamic false)

p :: Free SpaceF Unit -> Free SpaceF Unit
p = rDOMNode "p" [] (IsDynamic false)

param :: Free SpaceF Unit -> Free SpaceF Unit
param = rDOMNode "param" [] (IsDynamic false)

picture :: Free SpaceF Unit -> Free SpaceF Unit
picture = rDOMNode "picture" [] (IsDynamic false)

pre :: Free SpaceF Unit -> Free SpaceF Unit
pre = rDOMNode "pre" [] (IsDynamic false)

progress :: Free SpaceF Unit -> Free SpaceF Unit
progress = rDOMNode "progress" [] (IsDynamic false)

q :: Free SpaceF Unit -> Free SpaceF Unit
q = rDOMNode "q" [] (IsDynamic false)

rp :: Free SpaceF Unit -> Free SpaceF Unit
rp = rDOMNode "rp" [] (IsDynamic false)

rt :: Free SpaceF Unit -> Free SpaceF Unit
rt = rDOMNode "rt" [] (IsDynamic false)

ruby :: Free SpaceF Unit -> Free SpaceF Unit
ruby = rDOMNode "ruby" [] (IsDynamic false)

s :: Free SpaceF Unit -> Free SpaceF Unit
s = rDOMNode "s" [] (IsDynamic false)

samp :: Free SpaceF Unit -> Free SpaceF Unit
samp = rDOMNode "samp" [] (IsDynamic false)

script :: Free SpaceF Unit -> Free SpaceF Unit
script = rDOMNode "script" [] (IsDynamic false)

section :: Free SpaceF Unit -> Free SpaceF Unit
section = rDOMNode "section" [] (IsDynamic false)

select :: Free SpaceF Unit -> Free SpaceF Unit
select = rDOMNode "select" [] (IsDynamic false)

small :: Free SpaceF Unit -> Free SpaceF Unit
small = rDOMNode "small" [] (IsDynamic false)

source :: Free SpaceF Unit -> Free SpaceF Unit
source = rDOMNode "source" [] (IsDynamic false)

span :: Free SpaceF Unit -> Free SpaceF Unit
span = rDOMNode "span" [] (IsDynamic false)

strong :: Free SpaceF Unit -> Free SpaceF Unit
strong = rDOMNode "strong" [] (IsDynamic false)

style :: Free SpaceF Unit -> Free SpaceF Unit
style = rDOMNode "style" [] (IsDynamic false)

sub :: Free SpaceF Unit -> Free SpaceF Unit
sub = rDOMNode "sub" [] (IsDynamic false)

summary :: Free SpaceF Unit -> Free SpaceF Unit
summary = rDOMNode "summary" [] (IsDynamic false)

sup :: Free SpaceF Unit -> Free SpaceF Unit
sup = rDOMNode "sup" [] (IsDynamic false)

table :: Free SpaceF Unit -> Free SpaceF Unit
table = rDOMNode "table" [] (IsDynamic false)

tbody :: Free SpaceF Unit -> Free SpaceF Unit
tbody = rDOMNode "tbody" [] (IsDynamic false)

td :: Free SpaceF Unit -> Free SpaceF Unit
td = rDOMNode "td" [] (IsDynamic false)

textarea :: Free SpaceF Unit -> Free SpaceF Unit
textarea = rDOMNode "textarea" [] (IsDynamic false)

tfoot :: Free SpaceF Unit -> Free SpaceF Unit
tfoot = rDOMNode "tfoot" [] (IsDynamic false)

th :: Free SpaceF Unit -> Free SpaceF Unit
th = rDOMNode "th" [] (IsDynamic false)

thead :: Free SpaceF Unit -> Free SpaceF Unit
thead = rDOMNode "thead" [] (IsDynamic false)

time :: Free SpaceF Unit -> Free SpaceF Unit
time = rDOMNode "time" [] (IsDynamic false)

title :: Free SpaceF Unit -> Free SpaceF Unit
title = rDOMNode "title" [] (IsDynamic false)

tr :: Free SpaceF Unit -> Free SpaceF Unit
tr = rDOMNode "tr" [] (IsDynamic false)

track :: Free SpaceF Unit -> Free SpaceF Unit
track = rDOMNode "track" [] (IsDynamic false)

u :: Free SpaceF Unit -> Free SpaceF Unit
u = rDOMNode "u" [] (IsDynamic false)

ul :: Free SpaceF Unit -> Free SpaceF Unit
ul = rDOMNode "ul" [] (IsDynamic false)

var :: Free SpaceF Unit -> Free SpaceF Unit
var = rDOMNode "var" [] (IsDynamic false)

video :: Free SpaceF Unit -> Free SpaceF Unit
video = rDOMNode "video" [] (IsDynamic false)

wbr :: Free SpaceF Unit -> Free SpaceF Unit
wbr = rDOMNode "body" [] (IsDynamic false)
