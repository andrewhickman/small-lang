param(
    [Parameter(Mandatory)]
    [string]
    $Test
)

cargo build --bin small-lang
cmd /c target\debug\small-lang.exe --color never check "ui/$Test.sl" 2> "ui/$Test.out"