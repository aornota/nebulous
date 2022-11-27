module Aornota.Nebulous.Persistence

open Aornota.Nebulous.Literals.Application
open Aornota.Nebulous.Literals.FileExtensions
open Aornota.Nebulous.Literals.Folders
open System
open System.Drawing
open System.Drawing.Imaging
open System.IO
open Thoth.Json.Net

type PersistenceType =
    | Preferences
    | Image

type ReadError =
    | FileNotFound
    | Other of string

[<Literal>]
let private JSON_SPACE_COUNT = 4

let private extraCoders = Extra.empty |> Extra.withInt64

let private desktopFolder =
    Environment.GetFolderPath(Environment.SpecialFolder.Desktop)

let private persistenceRoot =
    let folder = Path.Combine(desktopFolder, APPLICATION_NAME)

    if not (Directory.Exists(folder)) then
        Directory.CreateDirectory(folder)
    else
        DirectoryInfo(folder)

let private subFolder =
    function
    | Preferences -> None
    | Image -> Some FOLDER_IMAGES

let private folder persistenceType =
    match subFolder persistenceType with
    | Some subFolder ->
        let folder = Path.Combine(persistenceRoot.FullName, subFolder)

        if not (Directory.Exists(folder)) then
            Directory.CreateDirectory(folder)
        else
            DirectoryInfo(folder)
    | None -> persistenceRoot

let fileExtension =
    function
    | Preferences -> FILE_EXTENSION_PREFERENCES
    | Image -> FILE_EXTENSION_BMP

let read<'a> persistenceType name =
    async {
        let file = Path.Combine((folder persistenceType).FullName, name)

        if not (File.Exists(file)) then
            return Error FileNotFound
        else
            try
                let! json = File.ReadAllTextAsync(file) |> Async.AwaitTask

                return
                    match Decode.Auto.fromString<'a> (json, extra = extraCoders) with
                    | Ok data -> Ok data
                    | Error error -> Error(Other error)
            with exn ->
                return Error(Other exn.Message)
    }

let write persistenceType name (data: 'a) =
    async {
        let file = Path.Combine((folder persistenceType).FullName, name)

        let json = Encode.Auto.toString<'a> (JSON_SPACE_COUNT, data, extra = extraCoders)

        try
            do! File.WriteAllTextAsync(file, json) |> Async.AwaitTask
            return Ok()
        with exn ->
            return Error exn.Message
    }

let writeImage (generation: int) (bitmap: Bitmap) =
    let file =
        Path.Combine(
            (folder Image).FullName,
            $"""{DateTime.UtcNow.ToString("yyMMdd-HHmmss")} - {bitmap.Width}x{bitmap.Height} [{generation}].{fileExtension Image}"""
        )

    try
        // TODO-NMB: Why does ImageFormat.Png seemingly not work?...
        bitmap.Save(file, ImageFormat.Bmp)
        Ok()
    with exn ->
        Error exn.Message
