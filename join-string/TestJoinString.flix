/////////////////////////////////////////////////////////////////////////////
// new                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def new01(): JoinString = JoinString.new()

/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def toString01(): Bool = 
    let js = JoinString.new();
    JoinString.toString(js) == ""

@test
def toString02(): Bool = 
    let js = JoinString.new();
    let js1 = JoinString.appendString(js, "hello");
    JoinString.toString(js1) == "hello"

@test
def toString03(): Bool = 
    let js = JoinString.new();
    let js1 = JoinString.appendString(js, "hello");
    let js2 = JoinString.appendString(js1, " world");
    JoinString.toString(js2) == "hello world"

    