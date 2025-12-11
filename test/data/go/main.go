package main

import (
	"import1"
	"import2"
	"import3"
)

type MyIdent1 struct {
	a int
	b int
}

type MyIdent2[T any] struct {
	data []T
}

type MyIdent3 interface {
	Ident4(a int) string
}

// comment 1

func ident5() {

}

func (mi *MyIdent1) ident6 () {

}

type myIdent7 = int
type myIdent8 string

func MyIdent9[T any](s []T) {
    for _, v := range s {
        fmt.Print(v)
    }
}

type myIdent7Fake = float
