# GymPicker

GymPicker helps you automatically pick a Codeforces gym based on certain criteria,
like having a tutorial, difficulty between 3 and 4 stars, and standard input/output.

Currently, there are no configuration files and changing the selection criteria has to be
done directly in the source code. This shouldn't be too difficult, however, as you only need to
change some lines in the beginning of `Main.fs`. This may or may not change in the future.

## Building
`dotnet build`

## Running
`dotnet run`