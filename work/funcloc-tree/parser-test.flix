


@test
def temp01(): Result[String, String] = 
    let input = "   |   |   |---   101125455          11 KV Supply Transformer                 LANGS-E-HV";
    StringParser.run(HierarchyParser.indenting(), input)


@test
def temp02(): Result[String, String] = 
    let input = "   |   |--    LANGS-E-LG                               Low Voltage Generation                   LANGS-E";
    let parser = 
        StringParser.seqRight(
            HierarchyParser.indenting(),
            HierarchyParser.funcLoc()
        );
    StringParser.run(parser, input)

@test 
def temp03(): Result[String, String] = 
    let input = "$14006";    
    StringParser.run(HierarchyParser.equipmentId(), input)

@test 
def temp04(): Result[String, String] = 
    let input = "100140063";
    StringParser.run(HierarchyParser.equipmentId(), input)


@test
def funcLocHeaderLine01(): Result[(String, DateStamp), String] = 
    let input = "Functional Location  LANGS                             Valid From      20.02.20";
    StringParser.run(HierarchyParser.funcLocHeaderLine(), input)

@test
def descriptionHeaderLine02(): Result[String, String] = 
    let input = "Description     Langstone Substation";
    StringParser.run(HierarchyParser.descriptionHeaderLine(), input)

@test 
def flocRootLine01(): Result[FlocRoot, String] = 
    let input = "  LANGS                                    Langstone Substation";
    StringParser.run(HierarchyParser.flocRootLine(), input)

@test 
def equiLine01(): Result[EquiRow, String] = 
    let input = "   |   |   |---   101063582          Battery Tripping Set                     LANGS-E-HV";
    StringParser.run(HierarchyParser.equiLine(), input)


@test 
def equiLine02(): Result[EquiRow, String] = 
    let input = "|       |               |---   101157802          Trench                                   LANGS-RWM-IMP-DMS-SYS02-DAM01            101023916";
    StringParser.run(HierarchyParser.equiLine(), input)

@test 
def flocLine01(): Result[FlocRow, String] = 
    let input = "|       |       |--    LANGS-RWM-IMP-DMS-SYS02-DAM01            Dam                                      LANGS-RWM-IMP-DMS-SYS02";
    StringParser.run(HierarchyParser.flocLine(), input)

