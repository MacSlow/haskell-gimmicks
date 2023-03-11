HC    = ghc
FLAGS = -dynamic -O2 -threaded -Wall -Werror -rtsopts

APP1   = 3d-in-2d
SRC1   = 3d-in-2d.hs

APP2  = curves
SRC2  = curves.hs

APP3  = lorenz-attractor
SRC3  = lorenz-attractor.hs

APP4  = random-points-on-sphere
SRC4  = random-points-on-sphere.hs

APP5  = bezier-surface
SRC5  = bezier-surface.hs

APP6  = random-points-on-plane
SRC6  = random-points-on-plane.hs

all: $(APP1) $(APP2) $(APP3) $(APP4) $(APP5) $(APP6)

$(APP1): $(SRC1)
	$(HC) $(FLAGS) --make $< -o $@
	strip $@

$(APP2): $(SRC2)
	$(HC) $(FLAGS) --make $< -o $@
	strip $@

$(APP3): $(SRC3)
	$(HC) $(FLAGS) --make $< -o $@
	strip $@

$(APP4): $(SRC4)
	$(HC) $(FLAGS) --make $< -o $@
	strip $@

$(APP5): $(SRC5)
	$(HC) $(FLAGS) --make $< -o $@
	strip $@

$(APP6): $(SRC6)
	$(HC) $(FLAGS) --make $< -o $@
	strip $@

clean:
	rm *.o *.hi $(APP1) $(APP2) $(APP3) $(APP4) $(APP5) $(APP6)

