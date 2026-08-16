var max: i8 = 127
var overflow: i8 = max + 1

Wraps to -128 instead of trapping but overflow is suppose to be a sixth MIR check beside Bounds, SliceBounds, Null, etc but its not
