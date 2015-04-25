module examHaskell where
	nums = 1:3:(zipWith (+) nums (tail nums))