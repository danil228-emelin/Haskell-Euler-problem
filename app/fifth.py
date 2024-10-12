def compute():
	result = sum(i for i in range(2, 1000000) if i == fifthPower_digitSum(i))
	return str(result)

def fifthPower_digitSum(n):
	return sum(int(x)**5 for x in str(n))
print("\nSum of all the numbers that can be written as the sum of fifth powers of their digits:")
print(compute())

if __name__ == '__main__':
    fifthPower_digitSum