#lang pfsh

(wsl.exe whoami > me)

(wsl.exe echo me)

; (wsl.exe whoami > me you)

; (wsl.exe whoami > <)

(wsl.exe ls "-1" > files)

files

(wsl.exe wc -l < files > lines)

lines

;(define x "hello")
;x
;(wsl.exe echo x)