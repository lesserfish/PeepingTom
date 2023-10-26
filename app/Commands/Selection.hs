module Commands.Selection where

import Commands.Base
import Commands.Selection.Eq
import Commands.Selection.Geq
import Commands.Selection.Gt
import Commands.Selection.Leq
import Commands.Selection.Lt
import Commands.Selection.Neq
import State

cmdHelp :: String
cmdHelp = "\n$: Extract candidates from regions of virtual memory.\n\nUsage:\n\n\t$ [Comparison] [Argument]\n\nExamples:\n\n\t$ == 3\n\t$ <= 9\n\t$ > 127\n\n"

cmdCommand :: Command
cmdCommand =
    HCommand
        { cName = "$"
        , subcommands = [eqCommand, geqCommand, leqCommand, gtCommand, ltCommand, neqCommand]
        , help = cmdHelp
        }
