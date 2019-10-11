import pygame, sys, math
from pygame.locals import *
from Vec2d import Vec2d
import copy
import numbers

##
# Object world
#
__unit_pt        = (0.0, 0.0)               # cartesian coord: (x, y)
__unit_vec       = Vec2d((1.0, 0.0))        # 2d cartesian vector: (x=1.0, y=0.0)
__unit_polar_vec = (1.0, 0.0)               # 2d polar vector: (r=1.0, theta=0.0rad)
__unit_line      = (__unit_pt, __unit_vec)  # line = point + vector
__unit_segment   = (__unit_pt, __unit_pt)   # segment = point A + point B
__unit_circle    = (__unit_pt, 1.0)         # circle = point + radius
__unit_square    = (__unit_pt, 1.0)         # square = lower left point + size

def __is_scalar(__obj):
	return isinstance(__obj, numbers.Number)

def __is_angle(__obj):
	return isinstance(__obj, numbers.Number)

def __is_normalized_rad_angle(__obj):
	return (__is_angle(__obj) and __obj>=0.0 and __obj<2*math.pi)

def __is_normalized_deg_angle(__obj):
	return (__is_angle(__obj) and __obj>=0.0 and __obj<360.0)

def __normalize_angle(__angle, circ):
	return __angle % circ

def __normalize_rad_angle(__angle):
	return __normalize_angle(__angle, 2*math.pi)

def __normalize_deg_angle(__angle):
	return __normalize_angle(__angle, 360.0)

def __normalize_unit_angle(__unit_angle):
	return __normalize_angle(__angle, 360.0)

def __is_pt(__obj):
	return (isinstance(__obj, tuple) and len(__obj) == 2 and
			__is_scalar(__obj[0])  and __is_scalar(__obj[1]))

def __is_vec(__obj):
	return isinstance(__obj, Vec2d)

def __is_line(__obj):
	return (isinstance(__obj, tuple) and
			len(__obj) == 2 and __is_pt(__obj[0]) and __is_vec(__obj[1]))

def __is_segment(__obj):
	return (isinstance(__obj, tuple) and
			len(__obj) == 2 and __is_pt(__obj[0]) and __is_pt(__obj[1]))

def __is_circle(__obj):
	return (isinstance(__obj, tuple) and
			len(__obj) == 2 and __is_pt(__obj[0]) and __is_scalar(__obj[1]))

def __is_square(__obj):
	return (isinstance(__obj, tuple) and
			len(__obj) == 2 and __is_pt(__obj[0]) and __is_scalar(__obj[1]))

def __new_obj(__unit_obj):
	return copy.deepcopy(__unit_obj)

##
# Custom objects

# Pad
__unit_pad = (__new_obj(__unit_vec), 1.0)

def __is_pad(__obj):
	return (isinstance(__obj, tuple) and len(__obj) == 2 and
			__is_vec(__obj[0]) and __is_scalar(__obj[1]))

##
# Model world
#

# Transformations
def _T(__obj, __vec):
	""" Translation """
	if __is_pt(__obj):
		return (__obj[0]+__vec[0], __obj[1]+__vec[1])

	elif __is_vec(__obj):
		return __obj.__iadd__(__vec)
	
	elif __is_segment(__obj):
		return (_T(__obj[0], __vec), _T(__obj[1], __vec))
	
	elif __is_line(__obj) or __is_circle(__obj) or __is_square(__obj):
		return (_T(__obj[0], __vec), __obj[1])
	
	elif __is_pad(__obj):
		return (_T(__obj[0], __vec), __obj[1])
	
	else:
		raise TypeError('Cannot operate translation on object %s' % str(__obj))

def _R(__obj, theta):
	""" Rotation """
	if __is_vec(__obj):
		__obj.rotate(theta)
		return __obj
		
	elif __is_pad(__obj):
		return (_R(__obj[0], theta), __obj[1])
	
	else:
		raise TypeError('Cannot operate rotation on object %s' % str(__obj))
	

def _S(__obj, scale_x, scale_y):
	""" Scaling """
	if __is_vec(__obj):
		return __obj.imul((scale_x, scale_y))
	
	else:
		raise TypeError('Cannot operate scaling on object %s' % str(__obj))
	
if __name__ == "__main__":
	pass
