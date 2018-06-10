# huey-interpreter
 Language Grammar

Here is the BNF description of the language:

     <color> ::= (rgb <byte> <byte> <byte> )
               | <varref>
               | ( <unary-op> <color> )
               | ( <color> <2color-op> <color> )
               | ( <color> <1color-op> <number> )
               | ( color <var> = <color> in <color> )
               | ( do <assignment>* <color> )          




    <assignment> ::= ( <varref> <= <color> )               

    <unary-op> ::= invert | darker

    <2color-op> ::= + | - | mix

    <1color-op> ::= * | shift

A <number> can be any real number, positive or negative, including integers. As noted above, a <byte> is an 8-bit integer in the range [0..255]. The semantics of the values and operators is defined below.

The symbols color, do, in, =, and <= are keywords.

Example Expressions

These are all legal Huey expressions:

    white
    (rgb 0 255 0)
    (invert (rgb 4 4 4))
    (rgb 255 0 255) mix ((rgb 0 255 0) + (rgb 4 4 4))
    ((rgb 255 0 255) shift -10)
    (color purple = ((rgb 255 0 0) mix (rgb 0 0 255))
        in (darker purple))
    (color green = (rgb 0 255 0)
        in (color blue-green = (green mix (rgb 0 0 255))
               in (invert blue-green)))
    (do (rgb 255 0 0))
    (color c = (rgb 0 255 0) in
       (color d = (rgb 0 0 255) in
          (do (c <= (c mix d))
              (d <= (c mix d))
              ((c mix d) shift 5))))

As the grammar indicates, expressions nest arbitrarily deeply.

Semantics

RGB Values

    An RGB value is a triple of three bytes, each an integer between 0 to 255, inclusive. These values can be implemented in any way that supports the operations of the language. From the users perspective, these are like literals in that they evaluate to themselves.

Unary Expressions

    A unary operator takes a color as its operand and returns a new color.

        (invert operand) returns a color whose RGB components are 255 minus the corresponding components of operand.
        (invert (rgb 150 99 42)) equals (rgb 105 156 213).

        (darker operand) returns a color whose RGB components are one-half the corresponding components of operand.
        (darker (rgb 150 99 42)) equals (rgb 75 49 21).

Two-Color Expressions

    These binary operators take two colors as their operands.

        (color1 + color2) returns a color whose RGB components are the sums of the corresponding components of its two operands.
        ((rgb 150 99 42) + (rgb 50 18 241)) equals (rgb 200 117 255).

        (color1 - color2) returns a color whose RGB components are the differences of the corresponding components of its two operands.
        ((rgb 150 99 42) - (rgb 50 108 21)) equals (rgb 100 0 21).

        (color1 mix color2) returns a color whose RGB components are 50-50 blends of the corresponding components of its two operands.
        ((rgb 150 99 42) mix (rgb 50 108 21)) equals (rgb 100 103 31).

One-Color Expressions

    This kind of binary operator takes a color as its left operand and a number as its right operand.

        (color * number) returns a color whose RGB components are number times the corresponding components of its color operand.
        ((rgb 150 99 42) * 1.6) equals (rgb 240 158 67).

        (color shift number) returns a color whose RGB components are number plus the corresponding components of its color operand.
        ((rgb 150 99 42) shift -50) equals (rgb 100 49 0).

Primitive Values

    The initial environment of a Huey program contains two primitive colors:

        white, bound to the value (rgb 255 255 255)
        black, bound to the value (rgb 0 0 0)

Blocks with Local Variables

    A variable reference is meaningful only within the body of the color/in expression that declares the variable. It is not meaningful in the value being assigned to the variable. A reference to a variable that has not been defined by a color/in expression is an error.

    A color/in expression behaves as follows:

        First, the value of the variable is evaluated in the current environment.
        Then, a new environment is created by extending the current environment with the new variable/value pair.
        Finally, the body of the expression is evaluated in the new environment.

Sequences with State -- new

    A do expression consists of 0 or more assignment statements followed by a color expression. The value of a do expression is the value of that final color.

    An assignment statement changes the value of an existing variable. An attempt to assign a value to a variable that has not been defined by a containing color/in expression is an error.

    A do expression behaves as follows:

        Evaluate the assignment statements in order. For each:
            evaluate the color expression on the righthand side of the statement, and
            update the value of the variable on the lefthand side in the current environment.
        Evaluate the final color expression in the current environment and return that value as the value of the do expression.

Syntactic Sugar

All of the features defined above are part of the core of the Huey language except:

    (color1 mix color2)
    ... which is a syntactic abstraction of ((color1 * 0.5) + (color2 * 0.5))

    (darker color)
    ... which is a syntactic abstraction of (color * 0.5)
