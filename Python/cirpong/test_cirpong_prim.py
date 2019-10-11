import cirpong_prim as cp

if __name__ == "__main__":
	print "Tests"

	print "Instances"
	o1 = cp.__new_obj(cp.__unit_pt)
	print o1
	o2 = cp.__new_obj(cp.__unit_vec)
	print o2
	o3 = cp.__new_obj(cp.__unit_line)
	print o3
	o4 = cp.__new_obj(cp.__unit_segment)
	print o4
	o5 = cp.__new_obj(cp.__unit_circle)
	print o5
	o6 = cp.__new_obj(cp.__unit_square)
	print o6
	o7 = cp.__new_obj(cp.__unit_pad)
	print o7

	print "Types"
	print cp.__is_pt(o1)
	print cp.__is_vec(o2)
	print cp.__is_line(o3)
	print cp.__is_segment(o4)
	print cp.__is_circle(o5)
	print cp.__is_square(o6)
	print cp.__is_pad(o7)

	print "Translation"
	o1 = cp._T(o1, (1, 1))
	print o1
	o2 = cp._T(o2, (1, 1))
	print o2
	o3 = cp._T(o3, (1, 1))
	print o3
	o4 = cp._T(o4, (1, 1))
	print o4
	o5 = cp._T(o5, (1, 1))
	print o5
	o6 = cp._T(o6, (1, 1))
	print o6
	o7 = cp._T(o7, (1, 1))
	print o7

	print "Rotation"
	try:
		cp._R(o1, 15)
		print o1
	except TypeError as e:
		print e

	try:
		cp._R(o2, 15)
		print o2
	except TypeError as e:
		print e
		
	try:
		cp._R(o3, 15)
		print o3
	except TypeError as e:
		print e
		
	try:
		cp._R(o4, 15)
		print o4
	except TypeError as e:
		print e
		
	try:
		cp._R(o5, 15)
		print o5
	except TypeError as e:
		print e
		
	try:
		cp._R(o6, 15)
		print o6
	except TypeError as e:
		print e

	try:
		cp._R(o7, 15)
		print o7
	except TypeError as e:
		print e
	
