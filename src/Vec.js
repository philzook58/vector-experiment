
exports.matVecArr = function(A){
	return function(x){
		var n = x.length
		var res = []
		for(var i =0; i < n; i++){
			var temp = 0
			for(var j = 0; j < n; j++){
				temp += A[n*i+j] * x[j]
			}
			res.push(temp)
		}
		return res

	}
}