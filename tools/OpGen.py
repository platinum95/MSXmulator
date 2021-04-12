#!/usr/bin/python3

import csv
from enum import Enum

FILENAME = "./OpList.csv"


class AddressingModes( Enum ):
    none = 0,
    registerA = 1,
    registerF = 2,
    registerAF = 3,
    registerB = 4,
    registerC = 5,
    registerBC = 6,
    registerD = 7,
    registerE = 8,
    registerDE = 9,
    registerH = 10,
    registerL = 11,
    registerHL = 12,
    registerIX = 13,
    registerIY = 14,
    registerSP = 15,
    registerPC = 16,

    PointerAF = 17,
    PointerBC = 18,
    PointerDE = 19,
    PointerHL = 20,

    PointerIX = 21,
    PointerIY = 22,
    PointerIXIndexed = 23,
    PointerIYIndexed = 24,

    IXLow = 25,
    IXHigh = 26,
    IYLow = 27,
    IYHigh = 28,
    PointerSP = 29,

    immediate = 30,
    relative = 31,

    FlagZeroSet = 32
    FlagZeroClear = 33,
    FlagCarrySet = 34,
    FlagCarryClear = 35,
    FlagParitySet = 36,
    FlagParityClear = 37,
    FlagNegativeSet = 38,
    FlagNegativeClear = 39,

    immediateExtended = 40,

# Dictionary mapping 8-bit addressing mode Enums with C++ strings
Read8UAddressingModeStringsByEnum = {
    AddressingModes.registerA: "AddressingModes::RegisterByteAccess<A, true>",
    AddressingModes.registerF: "AddressingModes::RegisterByteAccess<A, false>",
    AddressingModes.registerB: "AddressingModes::RegisterByteAccess<BC, true>",
    AddressingModes.registerC: "AddressingModes::RegisterByteAccess<BC, false>",
    AddressingModes.registerD: "AddressingModes::RegisterByteAccess<DE, true>",
    AddressingModes.registerE: "AddressingModes::RegisterByteAccess<DE, false>",
    AddressingModes.registerH: "AddressingModes::RegisterByteAccess<HL, true>",
    AddressingModes.registerL: "AddressingModes::RegisterByteAccess<HL, false>",
    AddressingModes.IXLow: "AddressingModes::RegisterByteAccess<IX, true>",
    AddressingModes.IXHigh: "AddressingModes::RegisterByteAccess<IX, false>",
    AddressingModes.IYLow: "AddressingModes::RegisterByteAccess<IY, true>",
    AddressingModes.IYHigh: "AddressingModes::RegisterByteAccess<IY, false>",
    
    AddressingModes.PointerSP: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterAccess<SP>, uint8_t>",
    AddressingModes.PointerAF: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<A>, uint8_t>",
    AddressingModes.PointerBC: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<BC>, uint8_t>",
    AddressingModes.PointerDE: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<DE>, uint8_t>",
    AddressingModes.PointerHL: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<HL>, uint8_t>",
    AddressingModes.PointerIX: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<IX>, uint8_t>",
    AddressingModes.PointerIY: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<IY>, uint8_t>",
    AddressingModes.PointerIXIndexed: "AddressingModes::DereferenceFromAddress<AddressingModes::IndexedIXAddress, uint8_t>",
    AddressingModes.PointerIYIndexed: "AddressingModes::DereferenceFromAddress<AddressingModes::IndexedIYAddress, uint8_t>",

    AddressingModes.immediate: "AddressingModes::Immediate<0>",
    AddressingModes.relative: "AddressingModes::Relative<0>",

    AddressingModes.FlagZeroSet: "AddressingModes::BitSet<AddressingModes::RegisterByteAccess<A, false>, Flags::Zero>",
    AddressingModes.FlagZeroClear: "AddressingModes::BitClear<AddressingModes::RegisterByteAccess<A, false>, Flags::Zero>",
    AddressingModes.FlagCarrySet: "AddressingModes::BitSet<AddressingModes::RegisterByteAccess<A, false>, Flags::Carry>",
    AddressingModes.FlagCarryClear: "AddressingModes::BitClear<AddressingModes::RegisterByteAccess<A, false>, Flags::Carry>",
    AddressingModes.FlagParitySet: "AddressingModes::BitSet<AddressingModes::RegisterByteAccess<A, false>, Flags::Parity>",
    AddressingModes.FlagParityClear: "AddressingModes::BitClear<AddressingModes::RegisterByteAccess<A, false>, Flags::Parity>",
    AddressingModes.FlagNegativeSet: "AddressingModes::BitSet<AddressingModes::RegisterByteAccess<A, false>, Flags::Negative>",
    AddressingModes.FlagNegativeClear: "AddressingModes::BitClear<AddressingModes::RegisterByteAccess<A, false>, Flags::Negative>",
    
}

# Dictionary mapping 16-bit addressing mode Enums with C++ strings
Read16UAddressingModeStringsByEnum = {
    AddressingModes.registerAF: "AddressingModes::RegisterWordAccess<A>",
    AddressingModes.registerBC: "AddressingModes::RegisterWordAccess<BC>",
    AddressingModes.registerDE: "AddressingModes::RegisterWordAccess<DE>",
    AddressingModes.registerHL: "AddressingModes::RegisterWordAccess<HL>",
    AddressingModes.registerIX: "AddressingModes::RegisterWordAccess<IX>",
    AddressingModes.registerIY: "AddressingModes::RegisterWordAccess<IY>",
    AddressingModes.registerSP: "AddressingModes::RegisterAccess<SP>",
    AddressingModes.registerPC: "AddressingModes::RegisterAccess<PC>",

    AddressingModes.PointerSP: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterAccess<SP>, uint16_t>",
    AddressingModes.PointerAF: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<A>, uint16_t>",
    AddressingModes.PointerBC: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<BC>, uint16_t>",
    AddressingModes.PointerDE: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<DE>, uint16_t>",
    AddressingModes.PointerHL: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<HL>, uint16_t>",
    AddressingModes.PointerIX: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<IX>, uint16_t>",
    AddressingModes.PointerIY: "AddressingModes::DereferenceFromAddress<AddressingModes::RegisterIndirectAddress<IY>, uint16_t>",

    AddressingModes.PointerIXIndexed: "AddressingModes::DereferenceFromAddress<AddressingModes::IndexedIXAddress, uint16_t>",
    AddressingModes.PointerIYIndexed: "AddressingModes::DereferenceFromAddress<AddressingModes::IndexedIYAddress, uint16_t>",

    AddressingModes.immediateExtended: "AddressingModes::ImmediateExtended<0>",
}

# Dictionary mapping 8-bit write-addressing mode Enums with C++ strings
Write8UAddressingModeStringsByEnum = {
    AddressingModes.registerA: "AddressingModes::WriteToRegister<A, true>",
    AddressingModes.registerF: "AddressingModes::WriteToRegister<A, false>",
    AddressingModes.registerB: "AddressingModes::WriteToRegister<BC, true>",
    AddressingModes.registerC: "AddressingModes::WriteToRegister<BC, false>",
    AddressingModes.registerD: "AddressingModes::WriteToRegister<DE, true>",
    AddressingModes.registerE: "AddressingModes::WriteToRegister<DE, false>",
    AddressingModes.registerH: "AddressingModes::WriteToRegister<HL, true>",
    AddressingModes.registerL: "AddressingModes::WriteToRegister<HL, false>",
    AddressingModes.IXLow: "AddressingModes::WriteToRegister<IX, true>",
    AddressingModes.IXHigh: "AddressingModes::WriteToRegister<IX, false>",
    AddressingModes.IYLow: "AddressingModes::WriteToRegister<IY, true>",
    AddressingModes.IYHigh: "AddressingModes::WriteToRegister<IY, false>",
    
    AddressingModes.PointerSP: "AddressingModes::WriteToAddress<AddressingModes::RegisterAccess<SP>, uint8_t>",
    AddressingModes.PointerAF: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<A>, uint8_t>",
    AddressingModes.PointerBC: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<BC>, uint8_t>",
    AddressingModes.PointerDE: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<DE>, uint8_t>",
    AddressingModes.PointerHL: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<HL>, uint8_t>",
    AddressingModes.PointerIX: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<IX>, uint8_t>",
    AddressingModes.PointerIY: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<IY>, uint8_t>",
    AddressingModes.PointerIXIndexed: "AddressingModes::WriteToAddress<AddressingModes::IndexedIXAddress, uint8_t>",
    AddressingModes.PointerIYIndexed: "AddressingModes::WriteToAddress<AddressingModes::IndexedIYAddress, uint8_t>",
}

# Dictionary mapping 16-bit write-addressing mode Enums with C++ strings
Write16UAddressingModeStringsByEnum = {
    AddressingModes.registerAF: "AddressingModes::WriteToRegister<A>",
    AddressingModes.registerBC: "AddressingModes::WriteToRegister<BC>",
    AddressingModes.registerDE: "AddressingModes::WriteToRegister<DE>",
    AddressingModes.registerHL: "AddressingModes::WriteToRegister<HL>",
    AddressingModes.registerIX: "AddressingModes::WriteToRegister<IX>",
    AddressingModes.registerIY: "AddressingModes::WriteToRegister<IY>",
    AddressingModes.registerSP: "AddressingModes::WriteToRegister<SP>",
    AddressingModes.registerPC: "AddressingModes::WriteToRegister<PC>",

    AddressingModes.PointerSP: "AddressingModes::WriteToAddress<AddressingModes::RegisterAccess<SP>, uint16_t>",
    AddressingModes.PointerAF: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<A>, uint16_t>",
    AddressingModes.PointerBC: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<BC>, uint16_t>",
    AddressingModes.PointerDE: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<DE>, uint16_t>",
    AddressingModes.PointerHL: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<HL>, uint16_t>",
    AddressingModes.PointerIX: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<IX>, uint16_t>",
    AddressingModes.PointerIY: "AddressingModes::WriteToAddress<AddressingModes::RegisterIndirectAddress<IY>, uint16_t>",

    AddressingModes.PointerIXIndexed: "AddressingModes::WriteToAddress<AddressingModes::IndexedIXAddress, uint16_t>",
    AddressingModes.PointerIYIndexed: "AddressingModes::WriteToAddress<AddressingModes::IndexedIYAddress, uint16_t>",
}


# Dictionary with key of opcode, tuple of implied write, readO1, readO2 addressing modes (only for 8-bit)
ImpliedAddressingByOp = {
    "ADD": (AddressingModes.registerA, AddressingModes.registerA, None),
    "ADC": (AddressingModes.registerA, AddressingModes.registerA, None),
    "SUB": (AddressingModes.registerA, AddressingModes.registerA, None),
    "SBC": (AddressingModes.registerA, AddressingModes.registerA, None),
    "AND": (AddressingModes.registerA, AddressingModes.registerA, None),
    "XOR": (AddressingModes.registerA, AddressingModes.registerA, None),
    "OR": (AddressingModes.registerA, AddressingModes.registerA, None),
    "CP": (AddressingModes.none, AddressingModes.registerA, None),
}

AddressingMnemonicToEnum = {
    "A": AddressingModes.registerA,
    "B": AddressingModes.registerB,
    "C": AddressingModes.registerC,
    "D": AddressingModes.registerD,
    "E": AddressingModes.registerE,
    "H": AddressingModes.registerH,
    "L": AddressingModes.registerL,

    "AF": AddressingModes.registerAF,
    "BC": AddressingModes.registerAF,
    "DE": AddressingModes.registerAF,
    "HL": AddressingModes.registerAF,

    "SP": AddressingModes.registerSP,
    "PC": AddressingModes.registerPC,

    "IXH": AddressingModes.IXHigh,
    "IXL": AddressingModes.IXLow,
    "IYH": AddressingModes.IYHigh,
    "IYL": AddressingModes.IYLow,

    "(AF)": AddressingModes.PointerAF,
    "(BC)": AddressingModes.PointerBC,
    "(DE)": AddressingModes.PointerDE,
    "(HL)": AddressingModes.PointerHL,
    "(IX)": AddressingModes.PointerIX,
    "(IY)": AddressingModes.PointerIY,

    "(IX+d)": AddressingModes.PointerIXIndexed,
    "(IY+d)": AddressingModes.PointerIYIndexed,

    "n": AddressingModes.immediate,
    "nn": AddressingModes.immediateExtended,
    "(nn)": AddressingModes.immediateExtended,

    "(PC+e)": AddressingModes.relative,

    "NZ": AddressingModes.FlagZeroClear,
    "Z": AddressingModes.FlagZeroSet,
    "NC": AddressingModes.FlagCarryClear,
    #"C": AddressingModes.FlagCarrySet,
    "PO": AddressingModes.FlagParityClear,
    "PE": AddressingModes.FlagParitySet,
    "N": AddressingModes.FlagNegativeClear,
    "M": AddressingModes.FlagNegativeSet,

}

AddressingModes16Bit = [
    AddressingModes.registerAF, AddressingModes.registerBC, AddressingModes.registerDE,
    AddressingModes.registerHL, AddressingModes.registerIX, AddressingModes.registerIY,
    AddressingModes.registerSP, AddressingModes.registerPC
]

class Operation:
    OperationName = ""
    OpCode = ""
    OpCodeHex = ""
    T = ""
    M = ""
    M1 = ""
    Notes = ""

    writeAddressing = None
    read1Addressing = None
    read2Addressing = None


    def __init__( self, opRow ):
        # opRow is a list of comma-separated strings
        opcode = opRow[ 0 ].split( ' ' )

        # Ignore extended instructions for now
        if ( len( opcode[ 0 ] ) != 2 ):
            return
        

        mnemonic = opRow[ 1 ].split( ' ' )

        self.OpCodeHex = opcode[ 0 ]
        self.OpCode = int( self.OpCodeHex, 16 )
        self.OperationName = mnemonic[ 0 ]
        if ( len( opRow ) > 2 ):
            self.T = opRow[ 2 ]
            self.M = opRow[ 3 ]
            self.M1 = opRow[ 4 ]
            if ( len( opRow ) > 5 ):
                self.Notes = opRow[ 5 ]
        
        # Parse addressing mode
        mnemonicLen = len( mnemonic )

        impliedAddressing = ImpliedAddressingByOp.get( self.OperationName, (None, None, None) )
        if ( mnemonicLen == 1 ):
            # No addressing modes specified in mnemonic, check if implied by op
            self.writeAddressing = impliedAddressing[ 0 ]
            self.read1Addressing = impliedAddressing[ 1 ]
            self.read2Addressing = impliedAddressing[ 2 ]
        
        else:
            # TODO - remove * from mnemonic
            # Op has addressing mode(s)
            addressingModes = mnemonic[ 1 ].split( ',' )
            assert( len( addressingModes ) < 3 )

            writeMode = impliedAddressing[ 0 ]
            readMode1 = impliedAddressing[ 1 ]
            readMode2 = None
            
            if ( readMode1 == None ):
                readMode1 = AddressingMnemonicToEnum.get( addressingModes[ 0 ], None )
                assert( readMode1 != None )

            if ( len( addressingModes ) == 2 and readMode2 == None ):
                readMode2 = AddressingMnemonicToEnum.get( addressingModes[ 1 ], None )
                assert( readMode2 != None )

            if ( writeMode == None ):
                writeMode = readMode1

            



    def getOpCallString( self ):
        opString = "case 0x" + self.OpCodeHex + ":{\n" + "\tOperationBase<Operations::"

        if ( self.writeAddressing == None and self.read1Addressing == None and self.read2Addressing == None ):
            opString += self.OperationName + ">();"
        
        else:
            if ( self.read1Addressing != None ):

                pass

        opString += "\n\tbreak;\n}"
        return opString
            





operations = list()

with open( FILENAME, newline='\n' ) as csvFile:
    opData = csv.reader( csvFile, delimiter='\t' )
    for op in opData:
        operations.append( Operation( op ) )

    # TODO - sort by opcode
    for op in operations:
        print( op.getOpCallString() )