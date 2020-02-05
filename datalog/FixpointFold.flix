rel Book(name: String)

def count[a](_: a, acc:Int32): Int32 = acc + 1

def bookFacts[r](): #{ Book | r} = #{
    Book("In Diamond Square").
    Book("Vampire in Love").
    Book("The Word Tree").
}


pub def testCount01(): Int32 = 
    fold Book 0 count (solve bookFacts())


def printBook(book: String, acc: Unit): Unit = 
    Console.printLine("___" + book + "___");
    acc

pub def testPrint01(): Unit = 
    fold Book () printBook (solve bookFacts())

pub def testPrint02(): Unit = 
    let ans = solve bookFacts();
    fold Book () printBook ans
