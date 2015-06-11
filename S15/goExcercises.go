package main

import (
	"fmt"
	"math"
	"golang.org/x/tour/pic"
	"strings"
	"golang.org/x/tour/wc"
	"golang.ord/x/xtour/reader"
	"io"
	"os"
)

type rot13Reader struct {
	r io.Reader
}

type IPAddr [4]byte

func Sqrt(x float64) float64 {
	z := 2.0
	stop := 0.0
	
	for {
		z = z - (z*z - x)/(2*z)
		if math.Abs(stop-z) < 1e-15 {
			break
		}
		stop = z
	}
	return stop
}

func Pic(dx, dy int) [][]uint8 {
	pic := make([][]uint8, dy)
	for i := 0; i < dy; i++ {
		pic[i] = make([]uint8, dx)
		for j := 0; j < dx; j++ {
			pic[i][j] = uint8(i^j+(i+j)/2+i*j)
		}
	}
	return pic
}

func WordCount(s string) map[string]int {
	words := strings.Fields(s)
	length := len(words)
	wordCount := make(map[string]int)
	for i := 0; i < length; i++ {
		(wordCount[words[i]])++
	}
	return wordCount
}

func fibonacci() func() int {
	x := 0
	y := 1
	return func() int {
		x,y = y,x+y
		return x
	}
}

func (p IPAddr) String() string {
    return fmt.Sprintf("%d.%d.%d.%d",p[0], p[1], p[2], p[3])
}

func (r MyReader) Read(b []byte) (n int, err error) {
     b[0] = 'A'
     return 1, nil
}

func (r *rot13Reader) Read(b []byte) (int,error) {
    l,e := r.r.Read(b)
    for i,c := range(b) {
        if c <= 'Z' && c >='A'  {
            b[i] = (c - 'A' + 13)%26 + 'A'
        }else if c >= 'a' && c <= 'z' {
            b[i] = (c - 'a' + 13)%26 + 'a'
        } 
    }
    return l, e
}

func main() {
	//fmt.Println(Sqrt(2))
	//fmt.Println(math.Sqrt(2))
	//pic.Show(Pic)
	//wc.Test(WordCount)

	/* f := fibonacci()
	for i := 0; i < 10; i++ {
		fmt.Println(f())
	} */

	/* addrs := map[string]IPAddr{
		"loopback":  {127, 0, 0, 1},
		"googleDNS": {8, 8, 8, 8},
	}
	for n, a := range addrs {
		fmt.Printf("%v: %v\n", n, a)
	} */

	//reader.Validate(MyReader{})

	s := strings.NewReader("Lbh penpxrq gur pbqr!")
	r := rot13Reader{s}
	io.Copy(os.Stdout, &r)
}

