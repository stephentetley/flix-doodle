/*
 * Copyright 2019 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace Pretty {

    pub enum Doc {
        case Empty,
        case Text(Str),
        case Line(Bool),
        case Cat(Doc,Doc),
        case Nest(Int32,Doc),
        case Group(Doc),
        case Column(Int32 -> Doc),
        case Nesting(Int32 -> Doc)
    }

    enum SimpleDoc {
        case SEmpty,
        case SText(Str, SimpleDoc),
        case SLine(Str, SimpleDoc)
    }

    def extend(s: Str, n: Int32): Str = 
        s + String.repeat(" ", n)


    def flatten(columnPos: Int32, doc: Doc) : Doc =
        flattenHelper(columnPos, 0, doc, (x,y) -> (x,y)) |> snd

    def flattenHelper(column: Int32, nest: Int32, doc: Doc, cont : (Int32, Doc) -> (Int32, Doc)): (Int32, Doc) = 
        match doc {
            case Empty => cont(column, Empty)
            case Text(s) => cont(column + String.length(s), doc)
            case Cat(x,y) => 
                flattenHelper(column, nest, x, (col1,x1) -> 
                    flattenHelper(col1, nest, y, (col2, y1) -> 
                        cont(col2, Cat(x1, y1))))
            case Nest(n,x) => flattenHelper(column, nest+n, x, cont)
            case Line(true) => cont(column, Empty)
            case Line(false) => cont(column+1, Text(" "))
            case Group(x) => flattenHelper(column, nest, x, cont)
            case Column(fn) => flattenHelper(column, nest, fn(column), cont)
            case Nesting(fn) => flattenHelper(column, nest, fn(nest), cont)
        }

    def layout (width: Int32, doc: Doc): SimpleDoc = 
        layoutBest(width, 0, ("",doc) :: Nil, false, identity, _ -> SEmpty)

    def layoutBest(width: Int32, col: Int32, docs: List[(Str, Doc)], alternate: Bool, sk: SimpleDoc -> SimpleDoc, fk: Unit -> SimpleDoc): SimpleDoc = 
        match docs {
            case Nil => sk(SEmpty)
            case (_, Empty) :: rs => layoutBest(width, col, rs, alternate, sk, fk)
            case (iz, Cat(x,y)) :: rs =>
                layoutBest(width, col, (iz,x) :: (iz,y) :: rs, alternate, sk, fk)
            case (iz, Nest(n,x)) :: rs => 
                layoutBest(width, col, (extend(iz,n),x) :: rs, alternate, sk, fk)
            case (iz, Line(_)) :: rs =>
                let sk1 = v1 -> sk(SLine(iz, v1));
                layoutBest(width, String.length(iz), rs, alternate, sk1, fk)
            case (iz, Group(x)) :: rs =>
                let fk1 = _ -> layoutBest(width, col, (iz, x) :: rs, alternate, sk, fk);
                layoutBest(width, col, (iz, flatten(col,x)) :: rs, true, sk, fk1)
            case (_, Text(t)) :: rs =>
                if ((width >= 0) && alternate && isTooBig(t,col,width))
                    fk()
                else {
                    let sk1 = v1 -> sk(SText(t, v1));
                    layoutBest(width, col + String.length(t), rs, alternate, sk1, fk)
                }
            case (iz, Column(fn)) :: rs =>
                layoutBest(width, col, (iz, fn(col)) :: rs, alternate, sk, fk)
            case (iz, Nesting(fn)) :: rs =>
                layoutBest(width, col, (iz, fn(String.length(iz))) :: rs, alternate, sk, fk)
        }

    def isTooBig(text: Str, col: Int32, width: Int32) : Bool = 
        col + String.length(text) > width


    ///
    /// Pretty print the document to a string.
    /// Lines are terminated with the operating systems default line terminator.
    ///
    pub def prettyPrint(width: Int32, doc: Doc) : Str = 
        let sb = StringBuilder.new();
        let simpleDoc = layout(width, doc);
        prettyPrintHelper(sb, simpleDoc, identity);
        StringBuilder.toString(sb)

    def prettyPrintHelper(sb: StringBuilder, sdoc: SimpleDoc, cont: Unit -> Unit) : Unit = 
        match sdoc {
            case SEmpty => cont()
            case SText(t,rest) => { 
                StringBuilder.appendStr(sb, t);
                prettyPrintHelper(sb, rest, cont)
            }
            case SLine(x,rest) => { 
                StringBuilder.appendLineSeparator(sb);
                StringBuilder.appendStr(sb, x);
                prettyPrintHelper(sb, rest, cont)
            }
        }


    // ************************************************************************
    // Primitive printers 
    
    ///
    /// The empty document
    ///
    pub def empty(): Doc = Empty

    ///
    /// `nest` renders the document `doc` with the current indentation level 
    /// increased by `i`
    ///
    pub def nest(i: Int32, doc: Doc): Doc = Nest (i, doc)

    ///
    /// Generate the document containing the literal string `s`.
    /// The input text should not contain newline characters.
    ///
    pub def text(s: Str): Doc = Text(s)

    ///
    /// Undocumented - used by `align`.
    /// `column` gives access to the current column position.
    ///
    pub def column(f: Int32 -> Doc): Doc = Column(f)

    ///
    /// Undocumented - used by `align`.
    /// `nesting` gives access to the current nesting position.
    /// The nesting position is not the same as the column position.
    pub def nesting(f: Int32 -> Doc): Doc = Nesting(f)

    ///
    /// Use the `group` combinator to specify alternate layouts.
    /// `(group doc)` undoes all linebreaks in doc.
    ///
    pub def group(doc: Doc): Doc = Group(doc)

    ////
    /// `line` advances to the next line and indents to the current nesting level.
    /// If the line break is undone by group `line` is rendered as a space.
    ///
    pub def line(): Doc = Line(false)
    
    ///
    /// `linebreak` advances to the next line and indents to the current nesting 
    /// level.
    /// If the line break is undone by group `linebreak` is rendered as empty.    
    ///
    pub def linebreak(): Doc = Line(true)

    ///
    /// This is 'char' in PPrint (Haskell).
    ///
    pub def character(ch: Char): Doc = 
        match ch {
            case '\n' => line()
            case '\r' => line()
            case _ => Char.toString(ch) |> text
        }
        
    ///
    /// `softline` behaves like `space` if the document it is part of fits the page.
    /// If it is too large it renders as `line`.
    ///
    pub def softline(): Doc = line() |> group

    ///
    /// `softbreak` behaves like `empty` if the document it is part of fits the page.
    /// If it is too large it renders as `line`.
    ///
    pub def softbreak(): Doc = linebreak() |> group

    ///
    /// Concatenate documents `x` and `y`.
    ///
    pub def beside(x: Doc, y: Doc): Doc = Cat(x,y)

    ///
    /// Concatenate two documents horizontally (no separating space).
    ///
    pub def <>(x: Doc, y: Doc): Doc = beside(x,y)

    ///
    /// Concatenate two documents horizontally with a separating space.
    ///
    pub def besideSpace(x: Doc, y: Doc): Doc = x <> character(' ') <> y

    ///
    /// Concatenate two documents horizontally with a separating space.
    /// 
    pub def <+>(x: Doc, y:Doc): Doc = besideSpace(x,y)

    ///
    /// Concatenate two documents with a soft line.
    /// This is (</>) in PPrint (Haskell).
    ///
    pub def <!>(x: Doc, y: Doc): Doc = x <> softline() <> y
    
    ///
    /// Concatenate two documents with a soft break.
    /// This is (<//>) in PPrint (Haskell).
    ///
    pub def <!!>(x: Doc, y: Doc): Doc = x <> softbreak() <> y

    ///
    /// Concatenate two documents separating with `line`.
    /// This is (<$>) in PPrint (Haskell).
    ///
    pub def <&>(x: Doc, y: Doc): Doc = x <> line() <> y

    

    ///
    /// Concatenate two documents separating with `linebreak`.
    /// This is (<$$>) in PPrint (Haskell).
    ///
    pub def <&&>(x: Doc, y: Doc): Doc = x <> linebreak() <> y

    ///
    /// Alias for `<&&>`
    ///
    pub def vlinebreak(x: Doc, y: Doc): Doc = x <&&> y


    // ************************************************************************
    // List concatenation 

    pub def foldDocs(op: (Doc, Doc) -> Doc, docs: List[Doc]): Doc = 
        match docs {
            case Nil => empty()
            case x :: rs => foldDocsHelper(op, x, rs, identity)
        }

    def foldDocsHelper(op: (Doc, Doc) -> Doc, acc: Doc, docs:List[Doc], cont: Doc -> Doc): Doc = 
        match docs {
            case Nil => cont(acc)
            case x :: rs => foldDocsHelper(op, op(acc,x), rs, cont)
        }

    pub def punctuate(sep: Doc, docs:List[Doc]): Doc =
        foldDocs((l,r) -> l <> sep <> r, docs)
    
    ///
    /// Punctuate the list of documents with the separator `sep` and bookend them 
    /// with `left` and `right`.
    ///
    pub def encloseSep(left:Doc, right:Doc, sep: Doc, docs: List[Doc]): Doc = 
        encloseSepHelper(sep, docs, left, d -> d <> right)

    def encloseSepHelper(sep: Doc, docs: List[Doc], acc: Doc, cont: Doc -> Doc): Doc = 
            match docs {
                case Nil => cont(acc)
                case x :: Nil => { 
                    let acc1 = acc <!!> x;
                    cont(acc1)
                }
                case x :: rs => {
                    let acc1 = acc <!!> x <> sep;
                    encloseSepHelper(sep, rs, acc1, cont)
                }
            }

    ///
    /// Enclose in parens and separate with comma (a,b,c,...)
    ///
    pub def tupled(docs: List[Doc]) : Doc = encloseSep(lparen(), rparen(), comma(), docs)

    ///
    /// Separate documents horizontally with a space.
    ///
    /// Note - seems to be a problem using (<+>)...
    ///
    pub def hsep(docs: List[Doc]): Doc = foldDocs((x,y) -> besideSpace(x,y), docs)

    ///
    /// Separate documents vertically with (<&>)
    ///
    pub def vsep(docs: List[Doc]): Doc = foldDocs((x,y) -> x <&> y, docs)

    ///
    /// Concatenate documents horizontally (no space).
    ///
    pub def hcat(docs: List[Doc]): Doc= foldDocs((x,y) -> x <> y, docs)

    ///
    /// Separate documents with (<&&>)
    ///
    pub def vcat(docs: List[Doc]): Doc = foldDocs((x,y) -> x <&&> y, docs)

    ///
    /// `(align d)` renders the document `d` with the nesting level set to 
    /// the current column.
    ///
    pub def align(doc: Doc) :Doc = 
        column(k -> nesting(i -> nest(k - i, doc)))

    ///
    /// Implement hanging indentation.
    ///
    pub def hang(i: Int32, doc: Doc): Doc = align(nest(i,doc))

    ///
    /// Indent the document `doc` with `i` spaces.
    ///
    pub def indent(i: Int32, doc: Doc): Doc = hang(i, repeatString(" ", i) <> doc)

    ///
    /// Print an Int8 literal.
    ///
    pub def int8(i: Int8): Doc = Util.showInt8(i) |> text

    ///
    /// Print an Int16 literal.
    ///
    pub def int16(i: Int16): Doc = Util.showInt16(i) |> text

    ///
    /// Print an Int32 literal.
    ///
    pub def int32(i: Int32): Doc = Util.showInt32(i) |> text

    ///
    /// Print an Int64 literal.
    ///
    pub def int64(i: Int64): Doc = Util.showInt64(i) |> text

    ///
    /// Print an float32 literal.
    ///
    pub def float32(i: Float32): Doc = Util.showFloat32(i) |> text

    ///
    /// Print an float64 literal.
    ///
    pub def float64(i: Float64): Doc = Util.showFloat64(i) |> text


    pub def repeatString(s: Str, n: Int32): Doc = 
        String.repeat(s, n) |> text

    pub def repeat(d: Doc, n: Int32): Doc = 
        List.repeat(d, n) |> hcat

    // ************************************************************************
    // Character printers

    ///
    /// Single left parenthesis: '('
    ///
    pub def lparen() : Doc = character('(')

    ///
    /// Single right parenthesis: ')'
    ///
    pub def rparen() : Doc = character(')')

    ///
    /// Single left angle: '<'
    ///
    pub def langle() : Doc = character('<')

    ///
    /// Single right angle: '>'
    ///
    pub def rangle() : Doc = character('>')

    ///
    /// Single left brace: '{'
    ///
    pub def lbrace(): Doc = character('{')
    
    ///
    /// Single right brace: '}'
    ///
    pub def rbrace(): Doc= character('}')
    
    ///
    /// Single left square bracket: '['
    ///
    pub def lbracket(): Doc = character('[')
    
    ///
    /// Single right square bracket: ']'
    ///
    pub def rbracket(): Doc = character(']')

    ///
    /// Single quote
    ///
    pub def squote(): Doc= character('\'')

    ///
    /// The document @dquote@ contains a double quote
    ///
    pub def dquote(): Doc = character('"')

    ///
    /// The document @semi@ contains a semi colon
    ///
    pub def semi(): Doc = character(';')

    ///
    /// The document @colon@ contains a colon
    ///
    pub def colon(): Doc = character(':')

    ///
    /// The document @comma@ contains a comma
    ///
    pub def comma() : Doc = character(',')

    ///
    /// The document @space@ contains a single space
    ///
    pub def space() : Doc = character(' ')

    ///
    /// The document @dot@ contains a single dot
    ///
    pub def dot(): Doc = character('.')

    ///
    /// The document @backslash@ contains a back slash
    ///
    pub def backslash(): Doc = character('\\')

    ///
    /// The document @equals@ contains an equal sign
    ///
    pub def equals(): Doc = character('=')

    ///
    /// Generate a document of n spaces
    ///
    pub def spaces(n:int): Doc = repeatString(" ", n)

    ///
    /// Enclose the document body between `l` (left) and `r` (right).
    ///
    pub def enclose(l: Doc, r: Doc, body: Doc): Doc = l <> body <> r

    ///
    /// Enclose in single quotes '...'
    ///
    pub def squotes(x: Doc): Doc = enclose(squote(), squote(), x)
    
    ///
    /// Enclose in double quotes "..."
    ///
    pub def dquotes(x: Doc): Doc = enclose(dquote(), dquote(), x)
    
    ///
    /// Enclose in angle braces {...}
    ///
    pub def braces(x: Doc): Doc = enclose(lbrace(), rbrace(), x)
    
    ///
    /// Enclose in square brackets (...)
    ///
    pub def parens(x: Doc): Doc = enclose(lparen(), rparen(), x)
    
    ///
    /// Enclose in angle brackets <...>
    ///
    pub def angles(x: Doc): Doc = enclose(langle(), rangle(), x)
    
    ///
    /// Enclose in square brackets [...]
    ///
    pub def brackets (x: Doc): Doc = enclose(lbracket(), rbracket(), x)
    
    
 }
