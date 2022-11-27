module Aornota.Nebulous.Simulation.Model

open Aornota.Nebulous.Simulation.Configuration

// Note: Type aliases defined to allow use of nameof(...) in error messages.
type PendingValidateConfiguration = bool
type Evolving = bool option

type CellState =
    | Alive of int * (float * float * float)
    | Dead of int
    | Nascent of int

type Cell = CellState * (byte * byte * byte)

type Showing =
    | BackgroundOnly
    | BackgroundAndForeground
    | ForegroundOnly

type State =
    { Dimensions: int * int
      Configuration: Configuration * int
      PendingValidateConfiguration: PendingValidateConfiguration
      Generation: int
      Cells: Cell[,]
      Biases: (float * float * float)[,]
      Evolving: Evolving
      PendingReset: bool
      Showing: Showing
      Rate: int
      RenderEveryOverride: int option
      AutoSaveEvery: int option }
