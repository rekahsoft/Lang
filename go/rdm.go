package main

import (
	"fmt"
	// "math"
	// "os"

	// "github.com/aws/aws-sdk-go/aws/session"
	// "github.com/aws/aws-sdk-go/service/s3"
)

type Point struct {
	X float64
	Y float64
}

func addPoints(points ...Point) Point {
	newPoint := Point{X: 0, Y: 0}
	for _, point := range points {
		newPoint = Point{X: newPoint.X + point.X, Y: newPoint.Y + point.Y}
	}
	return newPoint
}

//func (p Point) Add()

func main() {
	// if len(os.Args) < 2 {
	// 	fmt.Println("you must specify a bucket")
	// 	return
	// }

	// sess := session.Must(session.NewSession())

	// svc := session.Must(session.NewSession())
	// session.
	fmt.Println("Entry")

	points := []Point{Point{0,0},Point{1,0},Point{0,1},Point{0,0},Point{0,0}}
	newPoints := addPoints(points...)

	fmt.Println(points, newPoints)
}
