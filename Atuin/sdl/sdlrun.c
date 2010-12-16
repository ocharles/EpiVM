#include <stdio.h>
#include <SDL/SDL.h>
#include <SDL/SDL_gfxPrimitives.h>

#include <closure.h>

SDL_Surface* graphicsInit(int xsize, int ysize) {
    SDL_Surface *screen;

    if(SDL_Init(SDL_INIT_TIMER | SDL_INIT_VIDEO | SDL_INIT_AUDIO) <0 )
    {
	printf("Unable to init SDL: %s\n", SDL_GetError());
	return NULL;
    }

    screen = SDL_SetVideoMode(xsize, ysize, 32,
                              SDL_HWSURFACE | SDL_DOUBLEBUF);
    if (screen==NULL) {
	printf("Unable to init SDL: %s\n", SDL_GetError());
	return NULL;
    }

    return screen;
}

void filledRect(void *s_in,
	        int x, int y, int w, int h,
	        int r, int g, int b, int a) 
{
    SDL_Surface* s = (SDL_Surface*)s_in;
    Uint32 colour 
	= SDL_MapRGBA(s->format, (Uint8)r, (Uint8)g, (Uint8)b, (Uint8) a);
    SDL_Rect rect = { x, y, w, h };
    SDL_FillRect(s, &rect, colour);
}

void filledEllipse(void* s_in,
		   int x, int y, int rx, int ry,
                   int r, int g, int b, int a) 
{
    SDL_Surface* s = (SDL_Surface*)s_in;
    filledEllipseRGBA(s, x, y, rx, ry, r, g, b, a);
}

void drawLine(void* s_in,
	      int x, int y, int ex, int ey,
	      int r, int g, int b, int a) 
{
    SDL_Surface* s = (SDL_Surface*)s_in;
    lineRGBA(s, x, y, ex, ey, r, g, b, a);
}


void flipBuffers(void* s_in) {
    SDL_Surface* s = (SDL_Surface*)s_in;
    SDL_Flip(s);
}

void* startSDL(int x, int y) {
    SDL_Surface *s = graphicsInit(x, y);
    return (void*)s;
}

VAL KEY(int tag, SDLKey key) {
    VAL k;
    switch(key) {
    case SDLK_UP:
	k = CONSTRUCTOR(0,0,NULL);
	break;
    case SDLK_DOWN:
	k = CONSTRUCTOR(1,0,NULL);
	break;
    case SDLK_LEFT:
	k = CONSTRUCTOR(2,0,NULL);
	break;
    case SDLK_RIGHT:
	k = CONSTRUCTOR(3,0,NULL);
	break;
    default:
	k = CONSTRUCTOR1(4,MKINT((int)key));
	break;
    }
    return CONSTRUCTOR1(tag, k);
}

void* pollEvent() 
{
    SDL_Event event; // = (SDL_Event *) GC_MALLOC(sizeof(SDL_Event));
    int r = SDL_PollEvent(&event);
    if (r==0) {
	// FIXME: This will do something different depending on erasure...
	// Probably the only way is to generate C glue for an idris module?
	// Assuming erasure here.
	return CONSTRUCTOR(1,0,NULL); // Nothing
    }
    else {
	VAL ievent = NULL;
	switch(event.type) {
	case SDL_KEYDOWN:
	    ievent = KEY(0, event.key.keysym.sym);
	    break;
	case SDL_KEYUP:
	    ievent = KEY(1, event.key.keysym.sym);
	    break;
	case SDL_QUIT:
	    ievent = CONSTRUCTOR(2,0,NULL);
	    break;
	default:
	    // FIXME: This will do something different depending on erasure...
	    // Assuming erasure
	    return CONSTRUCTOR(1,0,NULL); // Nothing
	}
	// FIXME: This will do something different depending on erasure...
	// Assuming erasure
	return (void*)(CONSTRUCTOR1(0, ievent)); // Just ievent
    }
}

void pressAnyKey() 
{
    while(1) {
	SDL_Event event; // = (SDL_Event *) GC_MALLOC(sizeof(SDL_Event));
	SDL_WaitEvent(&event);
	if (event.type == SDL_KEYUP) { return; }
    }
}

void* waitEvent() 
{
    SDL_Event event; // = (SDL_Event *) GC_MALLOC(sizeof(SDL_Event));
    SDL_WaitEvent(&event);

    VAL ievent = NULL;
    switch(event.type) {
    case SDL_KEYDOWN:
	ievent = KEY(0, event.key.keysym.sym);
	break;
    case SDL_KEYUP:
	ievent = KEY(1, event.key.keysym.sym);
	break;
    case SDL_QUIT:
	ievent = CONSTRUCTOR(2,0,NULL);
	break;
    default:
	// FIXME: This will do something different depending on erasure...
	// Assuming erasure
	return CONSTRUCTOR(1,0,NULL); // Nothing
    }
    return (void*)(CONSTRUCTOR1(0, ievent)); // Just ievent
}

