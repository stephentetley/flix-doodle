
// @test
// def test01(): String = "false"

@test
def test01(): Result[(String, DateStamp), String] = 
    let input = "Functional Location  LANGS                             Valid From      20.02.20";
    StringParser.run(HierarchyParser.line1(), input)

@test
def test02(): Result[String, String] = 
    let input = "Description     Langstone Way";
    StringParser.run(HierarchyParser.line2(), input)

@test
def test03(): Result[String, String] = 
    let input = "   |   |   |---   101125455          11 KV Supply Transformer                 LANGS-E-HV";
    StringParser.run(HierarchyParser.indenting(), input)


@test
def test04(): Result[String, String] = 
    let input = "   |   |--    LANGS-E-LG                               Low Voltage Generation                   LANGS-E";
    let parser = 
        StringParser.seqRight(
            HierarchyParser.indenting(),
            HierarchyParser.floc()
        );
    StringParser.run(parser, input)

@test 
def test05(): Result[String, String] = 
    let input = "$14006";    
    StringParser.run(HierarchyParser.equiId(), input)

@test 
def test06(): Result[String, String] = 
    let input = "100140063";
    StringParser.run(HierarchyParser.equiId(), input)