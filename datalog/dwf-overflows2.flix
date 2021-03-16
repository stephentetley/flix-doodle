

def main(_args: Array[String]): Int32 & Impure =
    let r = getCharacteristics();
    r |> unsafeToString |> println;
    0 // exit code

pub rel Floc(name: String, osgb: String)
pub rel Easting(floc: String, value: Int32)

pub rel Characteristic(floc: String, classType: String, className: String, status: Int32, charName: String, charValue: String)

def flocs[r](): #{Floc | r} = #{
    Floc("SITE1-WWT-SOF-FLA", "NZ1000").
    Floc("SITE1-WWT-SOF-FLA-SYS01", "TE1000").
}

def getEasting(_osgb: String): String = ToString.toString(44917)
def getNorthing(_osgb: String): String = ToString.toString(50156)

/// (Pure) function calls and constants are allowed in rule heads...
def easting[r](): #{Characteristic, Floc | r} = #{
    Characteristic(floc, "003", "EAST_NORTH", 1, "EASTING", getEasting(east) ) :- 
        Floc(floc, east).
}


/// (Pure) function calls and constants are allowed in rule heads...
def northing[r](): #{Characteristic, Floc | r} = #{
    Characteristic(floc, "003", "EAST_NORTH", 1, "NORTHING", getNorthing(east) ) :- 
        Floc(floc, east).
}

pub def getCharacteristics[r](): #{ Characteristic | r } = 
    let r1 = project Characteristic solve (easting() <+> flocs());
    let r2 = project Characteristic solve (northing() <+> flocs());
    r1 <+> r2

