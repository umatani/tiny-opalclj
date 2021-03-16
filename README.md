# tiny-opalclj

## Install

1. Install [Racket](https://download.racket-lang.org).
1. Build from the source code.
    ```shell
    $ git clone https://github.com/umatani/tiny-opalclj.git
    $ cd tiny-opalclj
    $ raco pkg install
    ```

## Usage

1. Create a TJ source file.

    ```Java
    #lang tiny-opalclj
    #eval eval-abs
    
    class A {
      int f(int a) {
         return a + 1;
      }
    }
    ```

1. `require` the source from REPL.

1. To execute a method concretely, invoke it from REPL (`self` is set
   to a dummy object).

    ```racket
    > (A$f 100)
    ```
1. To interpret a method abstractly, specify a default parameter value
   for that method in the source and `require` it again.
  
    ```Java
    #lang tiny-opalclj
    #eval eval-abs

    class A {
      int f(int a = 3) {
         return a + 1;
      }
    }
    ```
