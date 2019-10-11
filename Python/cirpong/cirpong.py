import pygame, sys, math, random
from pygame.locals import *
from Vec2d import Vec2d
import cirpong_prim as cp

##
# Model world
MDL_ARENA_RADIUS = 1.0
MDL_OUT_TRACK    = 0.994
MDL_IN_TRACK     = 0.97
MDL_BALL_RADIUS  = 0.03
MDL_PAD_HEIGHT   = 0.15
MDL_PAD1_VELOCITY = 0.02
MDL_PAD2_VELOCITY = 0.02
MDL_PAD1_START_ANGLE   = 0.0
MDL_PAD2_START_ANGLE   = math.pi
MDL_DEFAULT_COLLISION_THRESHOLD = 0.8

##
# Game world
SCALE_FACTOR     = 350
SURFACE_X        = 1024
SURFACE_Y        = 768

# SPEED_FACTOR     = 20
ARENA_X          = 500
ARENA_Y          = 380
ARENA_RADIUS     = int(MDL_ARENA_RADIUS * SCALE_FACTOR)
BALL_RADIUS      = int(MDL_BALL_RADIUS * SCALE_FACTOR)
# ARENA_POS        = Vec2d((ARENA_X, ARENA_Y))

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

# the ball initial state
ballPosition = Vec2d((0.0, 0.0))
ballPolarPosition = ballPosition.to_polar()
#ballVelocity = Vec2d((0.005, 0.005))
ballVelocity = Vec2d((0.01, 0.0))
collision_threshold = MDL_DEFAULT_COLLISION_THRESHOLD


pad1 = [-1 * Vec2d((1.0, 0.0)).rotate_rad(MDL_PAD1_START_ANGLE),
        Vec2d((MDL_IN_TRACK, 0.0)).rotate_rad(MDL_PAD1_START_ANGLE - MDL_PAD_HEIGHT/2),
        Vec2d((MDL_OUT_TRACK, 0.0)).rotate_rad(MDL_PAD1_START_ANGLE - MDL_PAD_HEIGHT/2),
        Vec2d((MDL_OUT_TRACK, 0.0)).rotate_rad(MDL_PAD1_START_ANGLE + MDL_PAD_HEIGHT/2),
        Vec2d((MDL_IN_TRACK, 0.0)).rotate_rad(MDL_PAD1_START_ANGLE + MDL_PAD_HEIGHT/2)]

pad2 = [-1 * Vec2d((1.0, 0.0)).rotate_rad(MDL_PAD2_START_ANGLE),
        Vec2d((MDL_IN_TRACK, 0.0)).rotate_rad(MDL_PAD2_START_ANGLE - MDL_PAD_HEIGHT/2),
        Vec2d((MDL_OUT_TRACK, 0.0)).rotate_rad(MDL_PAD2_START_ANGLE - MDL_PAD_HEIGHT/2),
        Vec2d((MDL_OUT_TRACK, 0.0)).rotate_rad(MDL_PAD2_START_ANGLE + MDL_PAD_HEIGHT/2),
        Vec2d((MDL_IN_TRACK, 0.0)).rotate_rad(MDL_PAD2_START_ANGLE + MDL_PAD_HEIGHT/2)]

pad1Velocity = 0.0
pad1Angle = MDL_PAD1_START_ANGLE
pad2Velocity = 0.0
pad2Angle = MDL_PAD2_START_ANGLE

def toGameWorld(vec):
    v = vec * SCALE_FACTOR
    v[0] = int(v[0])
    v[1] = int(v[1])
    return  v + (ARENA_X, ARENA_Y)

# set up pygame
pygame.init()

# set up the window
windowSurface = pygame.display.set_mode((SURFACE_X, SURFACE_Y), 0, 32)
pygame.display.set_caption('CirPong')

def draw_arena():
    pygame.draw.circle(windowSurface, ARENA_COLOR,
                       (ARENA_X, ARENA_Y), ARENA_RADIUS, 0)
    pygame.draw.circle(windowSurface, ARENA_BORDER_COLOR,
                       (ARENA_X, ARENA_Y), ARENA_RADIUS, 1)
    
def drawBall():
    pygame.draw.circle(windowSurface, BALL_COLOR,
                       toGameWorld(ballPosition),
                       BALL_RADIUS, 0)

def eraseBall():
    pygame.draw.circle(windowSurface, ARENA_COLOR,
                       toGameWorld(ballPosition),
                       BALL_RADIUS, 0)

def moveBall():
    global ballPolarPosition
    ballPosition.__iadd__(ballVelocity)
    ballPolarPosition = ballPosition.to_polar()
    
def detectWallCollision():
    global ballVelocity
    if (ballPosition.get_distance((0.0, 0.0)) >= (1.0 - MDL_BALL_RADIUS)):
        pygame.quit()
        sys.exit()

def ballPolarCoord():
    return (ballPosition.get_length(), ballPosition.get_rad_angle())
        
def drawPad1():
    p = [toGameWorld(x) for x in pad1[1:]]
    pygame.draw.polygon(windowSurface, PAD_COLOR, p)

def erasePad1():
        pygame.draw.polygon(windowSurface, ARENA_COLOR, [toGameWorld(x) for x in pad1])
    
def movePad1():
    global pad1, pad1Angle
    pad1 = [x.rotate_rad(pad1Velocity) for x in pad1]
    pad1Angle += pad1Velocity

def detectPad1Collision():
    global ballVelocity, ballPolarPosition

    def ballInPadAngle():
        pad1HighAngle = pad1Angle + MDL_PAD_HEIGHT/2 + MDL_BALL_RADIUS
        pad1LowAngle  = pad1Angle - MDL_PAD_HEIGHT/2 - MDL_BALL_RADIUS
        return ballPolarPosition[1] <= pad1HighAngle and ballPolarPosition[1] >= pad1LowAngle

    def ballTouchesPad():
        return ballPolarPosition[0] >= MDL_IN_TRACK - MDL_BALL_RADIUS - 0.01

    def padNormal():
        return pad1[0]

    def getReflectionVec():
        v = ((2 * abs(padNormal().dot(ballVelocity))) * padNormal() + ballVelocity)
        print "Pad1"
        print "Ball vector: ", ballVelocity
        print "Reflection vector: ", v
        return v

    if (ballInPadAngle() and ballTouchesPad()):
        reflectVec = getReflectionVec()
        ballVelocity.set_x(reflectVec[0])
        ballVelocity.set_y(reflectVec[1])
        return True
    else:
        return False

    
def drawPad2():
    p = [toGameWorld(x) for x in pad2[1:]]
    pygame.draw.polygon(windowSurface, PAD_COLOR, p)

def erasePad2():
        pygame.draw.polygon(windowSurface, ARENA_COLOR, [toGameWorld(x) for x in pad2])
    
def movePad2():
    global pad2, pad2Angle
    pad2 = [x.rotate_rad(pad2Velocity) for x in pad2]
    pad2Angle += pad2Velocity

def detectPad2Collision():
    global ballVelocity, ballPolarPosition

    def ballInPadAngle():
        pad2HighAngle = pad2Angle + MDL_PAD_HEIGHT/2 + MDL_BALL_RADIUS
        pad2LowAngle  = pad2Angle - MDL_PAD_HEIGHT/2 - MDL_BALL_RADIUS
        return ballPolarPosition[1] <= pad2HighAngle and ballPolarPosition[1] >= pad2LowAngle

    def ballTouchesPad():
        return ballPolarPosition[0] >= MDL_IN_TRACK - MDL_BALL_RADIUS - 0.01

    def padNormal():
        return pad2[0]

    def getReflectionVec():
        v = ((2 * abs(padNormal().dot(ballVelocity))) * padNormal() + ballVelocity)
        print "Pad2"
        print "Ball vector: ", ballVelocity
        print "Reflection vector: ", v
        return v

    if (ballInPadAngle() and ballTouchesPad()):
        reflectVec = getReflectionVec()
        ballVelocity.set_x(reflectVec[0])
        ballVelocity.set_y(reflectVec[1])
        return True
    else:
        return False
    
def ballBeyondThreshold():
    return ballPolarPosition[0] > collision_threshold
    
if __name__ == "__main__":
    draw_arena()
    drawPad1()
    drawPad2()

    while True:
        eraseBall()
        moveBall()
        drawBall()
        if (ballBeyondThreshold()):
            if (not detectPad1Collision() and not detectPad2Collision()):
                detectWallCollision()
        if (pad1Velocity != 0):
            erasePad1()
            movePad1()
            drawPad1()
        if (pad2Velocity != 0):
            erasePad2()
            movePad2()
            drawPad2()
        pygame.display.update()

        for event in pygame.event.get():
            if (event.type == KEYDOWN):
                if (event.key == K_o):
                    pad1Velocity = MDL_PAD1_VELOCITY
                elif (event.key == K_i):
                    pad1Velocity = -MDL_PAD1_VELOCITY
                if (event.key == K_w):
                    pad2Velocity = MDL_PAD2_VELOCITY
                elif (event.key == K_e):
                    pad2Velocity = -MDL_PAD2_VELOCITY
                elif (event.key == K_q):
                    pygame.quit()
                    sys.exit()
            elif (event.type == KEYUP):
                if (event.key in [K_i, K_o]):
                    pad1Velocity = 0
                elif (event.key in [K_w, K_e]):
                    pad2Velocity = 0
            elif event.type == QUIT:
                pygame.quit()
                sys.exit()



