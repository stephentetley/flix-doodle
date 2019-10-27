

@test
def toString01() : Bool = 
    let sb = StringBuilder.new();
    StringBuilder.toString(sb) `assertEq!` ""