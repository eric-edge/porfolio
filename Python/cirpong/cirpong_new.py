import pygame, sys, math, random
from pygame.locals import *
from Vec2d import Vec2d
import cirpong_prim as cp
import inspect
import time

# available colors
BLACK              = (0, 0, 0)
WHITE              = (255, 255, 255)
RED                = (255, 0, 0)
GREEN              = (0, 255, 0)
BLUE               = (0, 0, 255)
GOLD               = (255, 215, 0)

# colors used
ARENA_COLOR        = BLUE
ARENA_BORDER_COLOR = GREEN
BALL_COLOR         = RED
PAD_COLOR          = GOLD
PADTRACK_COLOR     = BLACK

class generic_angle():
    """ Object holds an angle of any value """
    def __init__(self, angle):
        self.angle = angle

    def getState(self):
        return self.angle

    def setAngle(self, angle):
        self.angle = angle

    def __float__(self):
        return float(self.angle)

    def __neg__(self):
        return generic_angle(-self.angle)
    
    def __add__(self, other):
        if inspect.isclass(other) and issubclass(other, generic_angle):
            return generic_angle(self.angle + other.getState())
        else:
            return generic_angle(self.angle + other)

    def __iadd__(self, other):
        if inspect.isclass(other) and issubclass(other, generic_angle):
            self.angle += other.getState()
            return self
        else:
            self.angle += other
            return self
        
    def __sub__(self, other):
        if inspect.isclass(other) and issubclass(other, generic_angle):
            return generic_angle(self.angle - other.getState())
        else:
            return generic_angle(self.angle - other)
            
    def __isub__(self, other):
        if inspect.isclass(other) and issubclass(other, generic_angle):
            self.angle -= other.getState()
            return self
        else:
            self.angle -= other
            return self


class normalized_radian_angle(generic_angle):
    """ Object holds a radian angle in [0..2pi[ and snaps to 0 when within epsilon """
    epsilon = 0.0001
    
    def __init__(self, angle):
        self.epsilon = normalized_radian_angle.epsilon
        normalized_angle = normalized_radian_angle.normalize(angle)
        generic_angle.__init__(self, normalized_angle)

    def setAngle(self, angle):
        normalized_angle = self.normalize(angle)
        generic_angle.setAngle(self, normalized_angle)

    def __iadd__(self, other):
        if isinstance(other, normalized_radian_angle):
            self.setAngle(self.getState() + other.getState())
            return self
        else:
            self.setAngle(self.getState() + other)
            return self            

    def __add__(self, other):
        if isinstance(other, normalized_radian_angle):
            return normalized_radian_angle(self.getState() + other.getState())
        else:
            return normalized_radian_angle(self.getState() + other)

    @staticmethod
    def normalize(angle):
        newAngle = (angle % (2 * math.pi))
        if (newAngle < normalized_radian_angle.epsilon) or (((2 * math.pi) - newAngle) < normalized_radian_angle.epsilon):
            newAngle = 0.0
        return newAngle

class mdl_object(Vec2d):
    """
    Abstract object in the model world
    An object is represented by a 2D vector (Vec2d)
    All cartesian coord in [-1..1]X[-1..1] plan
    All angles normalized in [0..2pi[, snapped to 0 within epsilon
    +--------------------------+
    |            ^ +1          |
    |            |             |
    |            |             |
    |            |     .(x,y)  |
    |            |             |
    | -----------+---------->  |
    | -1        /|         +1  |
    |          / |             |
    |         /  |             |
    |  (x,y)./   |             |
    |            |-1           |
    +--------------------------+
    """
    def __init__(self, vec):
        Vec2d.__init__(self, vec)

    def getAngle(self):
        polarCoord = Vec2d.to_polar(self)
        return polarCoord[1]

    def distanceToOrigin(self):
        polarCoord = Vec2d.to_polar(self)
        return polarCoord[0]

class mdl_ball(mdl_object):
    _RADIUS  = 0.03
    _START_X = 0.0
    _START_Y = 0.0
    _VELOCITY = (1.0, 0.0)
    _SPEED = 0.01
    
    def __init__(self, x=_START_X, y=_START_Y, radius=_RADIUS, velocity=_VELOCITY, speed=_SPEED):
        mdl_object.__init__(self, (x, y))
        self.radius = radius
        self.velocity = Vec2d(velocity)
        self.speed = speed

    def getRadius(self):
        return self.radius

    def setRadius(self, radius):
        self.radius = radius

    def update(self, delta_t):
        self += self.velocity * self.speed

class mdl_pad(mdl_object):
    """
                  |
          Corners |
          0     1 |
       --- *---*  |
     L ^   |   |  |
     e |   | P |  |
     n |   | a |  * (x, y)
     g |   | d |  | 
     t v   |   |  |
     h --- *---*  |
          2     3 |
                  |
           ^   ^  ^
           |   |  |
           |   |   \_ Arena radius (1.0)
           |   |
           |    \_ Out track (0.994)
           |
            \_ In track (0.970)
         
    """
    _LENGTH       = 0.15
    _START_ANGLE  = 0.0
    _ANG_VELOCITY = 0.0
    _OUT_TRACK    = 0.994
    _IN_TRACK     = 0.97

    def __init__(self, angle=_START_ANGLE, length=_LENGTH):
        mdl_object.__init__(self, Vec2d((1.0, 0.0)).rotate_rad(angle))
        self.angle  = normalized_radian_angle(angle)
        self.length = length
        self.normal = -self
        self.angular_velocity = mdl_pad._ANG_VELOCITY
        self.corners = self.genCorners()
        
    def genCorners(self):
        """
        Pad at angle=0
        
              _ upperLeft corner
             /    _ upperRight corner
            /    /
            *---*
            |   |
            |   |
            |   |
            |   |
            |   |
            *---*
            |    \_ lowerRight corner
             \_ lowerLeft corner
         
        """
        upperLeft  = Vec2d((mdl_pad._IN_TRACK, 0.0))
        upperRight = Vec2d((mdl_pad._OUT_TRACK, 0.0))
        lowerLeft  = Vec2d((mdl_pad._OUT_TRACK, 0.0))
        lowerRight = Vec2d((mdl_pad._IN_TRACK, 0.0))
        
        return [ upperLeft.rotate_rad(self.angle + self.length/2),
                 upperRight.rotate_rad(self.angle + self.length/2),
                 lowerLeft.rotate_rad(self.angle - self.length/2),
                 lowerRight.rotate_rad(self.angle - self.length/2)  ]

    def getAngle(self):
        return self.angle
    
    def getLength(self):
        return self.length

    def setLength(self, length):
        self.length = length
        self.corners = self.genCorners()

    def getAngularVelocity():
        return self.angular_velocity
    
    def setAngularVelocity(self, angular_velocity):
        self.angular_velocity = angular_velocity

    def update(self, delta_t):
        if self.angular_velocity != 0:
            delta_angle = self.angular_velocity * delta_t
            self.rotate_rad(delta_angle)
            self.angle += delta_angle
            self.normal = -self
            self.corners = self.genCorners()


class mdl_world(object):
    def __init__(self):
        self.pad1 = mdl_pad(0.0)
        self.pad1Receiving = True
        self.pad2 = mdl_pad(math.pi)
        self.pad2Receiving = False
        self.ball = mdl_ball()
        self.padTouchRadius = ball._IN_TRACK - self.ball.getRadius()
        self.highAngle = self.pad1.getLength() + ballRadius
        self.wallTouchRadius = 1.0 - self.ball.getRadius()
        self.point = None

    def tick(self, delta_t):
        player1 = 1
        player2 = 2

        # Check for collisions
        if pad1Receiving and ballTouchesPad(self.pad1):
            self.reflectBall(self.pad1)
            pad2Receiving = True
            pad1Receiving = False
            
        elif pad2Receiving and ballTouchesPad(self.pad2):
            self.reflectBall(self.pad2)
            pad1Receiving = True
            pad2Receiving = False
            
        elif self.ballTouchesWall():
            if pad1Receiving:
                self.point = player2
            else:
                self.point = player1

        # Update world state
        self.pad1.update(delta_t)
        self.pad2.update(delta_t)
        self.ball.update(delta_t)


    def ballTouchesPad(self, pad):
        """
              __                |
            /    \              |
           | ball |             |
           |  o   | ------------|---- highAngle
           |      |             |
            \____/              |
              |      +------+ --|---- pad length
              |      |      |   |
              |      | Pad  |   |
              |      |      |   * --- pad angle
              |      |      |   |
              |      |      |   |
         -----|------+------+---|-->
              ^      ^      ^   |  x
              |      |      |   |
          (0.97 -   0.97  0.994 1.0
        ballRadius)
     
        """
        if self.ball.distanceToOrigin() >= self.touchRadius:
            refAngle = self.ball.getAngle() - pad.getAngle() + pad.getLength()/2.0
            return refAngle <= self.highAngle:
        else:
            return False

    def ballTouchesWall(self):
        return self.ball.distanceToOrigin() >= self.wallTouchRadius:

    def reflectBall(self, pad):
        self.ball[0] = 0.0
        self.ball[1] = 0.0
        self.ball.velocity *= -1

class game_world(object):
    _SCALE_FACTOR     = 350
    _ARENA_X          = 500
    _ARENA_Y          = 380

    def __init__(self, scale_factor=_SCALE_FACTOR, arena_x=_ARENA_X, arena_y=_ARENA_Y):
        self.scale_factor = scale_factor
        self.arena_x = arena_x
        self.arena_y = arena_y
        self.world = mdl_world()
        self.last_tick_time = 0

    def tick(self, delta_t):
        millis = int(round(time.time() * 1000))
        self.world.tick(millis)
        self.last_tick_time = millis

    def update(self):
        pass
        
if __name__ == '__main__':
    # MDL_PAD1_VELOCITY = 0.02
    # MDL_PAD2_VELOCITY = 0.02
    # MDL_PAD1_START_ANGLE   = 0.0
    # MDL_PAD2_START_ANGLE   = math.pi
    # MDL_DEFAULT_COLLISION_THRESHOLD = 0.8

    # SURFACE_X        = 1024
    # SURFACE_Y        = 768

    # a = generic_angle(math.pi)
    # print a.getState()
    # a = normalized_radian_angle(8 * math.pi)
    # print a.getState()
    # a = normalized_radian_angle((2 * math.pi) - 0.00001)
    # print a.getState()
    # print dir(a)

    print "*"
    b = mdl_ball()
    print b[0], b[1]

    print "**"
    b.update(.001)
    print b[0], b[1]

    print "***"
    p = mdl_pad()
    print p[0], p[1]

    print "*****"
    p.setAngularVelocity(.2)
    p.update(.001)
    print p
    print dir(p)
