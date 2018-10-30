title: Markdown所能识别代码高亮的语言
date: 2014-06-26 20:33:52
tags: markdown

---
##markdown能识别大约41语言,以下是一些例子


****

##支持的语言列表
<!--more-->
|Python |Ruby |Haml |Perl|PHP |Scala|Go|XML|HTML|Lasso|Markdown|AscllDoc|

|Django|Handlebars|CSS|SCSS|JSON|JavaScript|CoffeScript|ActionScript|VBScript|

|VB.NET|HTTP|Lua|AppleScript|Delphi|Oxygene|Java|C++|objectivec|Vala|C#|

|F#|OCaml|D|rsl|rib|MEL|GLSL|SQL|SmallTalk|Lisp|Clojure|

|ini|Apache|nginx|Diff|dos|bash|makefile|cmake|axapta|ruleslanguage|

|1c|avrasm|vhdl|parser3|livecodeserver|tex|brainfuck|haskell|erlang|

|erlang-repl|rust|matlab|scllab|r|mizar|mathematica|autohotkey|fix|
　
##Python
```Python
def somefunc(param1='', param2=0):
    r'''A docstring'''
    if param1 > param2: # interesting
        print 'Gre\'ater'
    return (param2 - param1 + 1 + 0b10l) or None

class SomeClass:
    pass

>>> message = '''interpreter
... prompt'''
```

##Ruby
```Ruby
class A < B; def self.create(object = User) object end end
class Zebra; def inspect; "X#{2 + self.object_id}" end end
module ABC::DEF
  include Comparable
  # @param test
  # @return [String] nothing
  def foo(test)
    Thread.new do |blockvar|
      ABC::DEF.reverse(:a_symbol, :'a symbol', :<=>, 'test' + ?\012)
      answer = valid?4 && valid?CONST && ?A && ?A.ord
    end.join
  end
```

##Perl
```Perl
sub load
{
  my $flds = $c->db_load($id,@_) || do {
    Carp::carp "Can`t load (class: $c, id: $id): '$!'"; return undef
  };
  my $o = $c->_perl_new();
  $id12 = $id / 24 / 3600;
  $o->{'ID'} = $id12 + 123;
  #$o->{'SHCUT'} = $flds->{'SHCUT'};
  my $p = $o->props;
  my $vt;
  $string =~ m/^sought_text$/;
  $items = split //, 'abc';
  $string //= "bar";
  for my $key (keys %$p)
  {
    if(${$vt.'::property'}) {
      $o->{$key . '_real'} = $flds->{$key};
      tie $o->{$key}, 'CMSBuilder::Property', $o, $key;
    }
  }
  $o->save if delete $o->{'_save_after_load'};
  return $o;
}
```

##PHP
```PHP
require_once 'Zend/Uri/Http.php';

namespace Location\Web;

interface Factory
{
    static function _factory();
}

abstract class URI extends BaseURI implements Factory
{
    abstract function test();

    /**
     * Returns a URI
     *
     * @return URI
     */
    static public function _factory($stats = array(), $uri = 'http')
    {
        echo __METHOD__;
        $uri = explode(':', $uri, 0b10);
        $schemeSpecific = isset($uri[1]) ? $uri[1] : '';
        $desc = 'Multi
line description';

        // Security check
        if (!ctype_alnum($scheme)) {
            throw new Zend_Uri_Exception('Illegal scheme');
        }

        return [
            'uri'   => $uri,
            'value' => null,
        ];
    }
}
```

#Scala
```Scala
object abstractTypes extends Application {
  abstract class SeqBuffer {
    type T; val element: Seq[T]; def length = element.length
  }
}

/** Turn command line arguments to uppercase */
object Main {
  def main(args: Array[String]) {
    val res = for (a <- args) yield a.toUpperCase
    println("Arguments: " + res.toString)
  }
}
```
