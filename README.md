# peakseek

A API for peak performance analysis and prediction based on simple ML.

Easy to use prediction and optimization for system performance which is data provided.

https://peakseek.net

## Usage

* PUT xs and ys.
```json
// PUT /data
{
  "xs":[1,2,3],
  "ys":[10,20,30]
}
```
```json
// PUT /data
// allowed below struct.
{
    "data":[
        {"x":1, "y":10},
        {"x":2, "y":20},
        {"x":3, "y":30}
    ]
}
```
* GET analyzed const or predicted data.
```json
// GET /const
{
  "const":{
    "a": 0,
    "b": 10
  }
}
```
```json
// GET /const/a
{
  "a":0,
}
```
```json

// GET /pred/4
{
  "x": 4,
  "py":40
}
```
* GET optimized peak.
```json
// GET /peak?xmin=0&xmax=5
{
  "x": 5,
  "py": 50
}
```
