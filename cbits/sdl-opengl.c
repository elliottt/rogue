#include <SDL.h>
#include <SDL_image.h>
#include <SDL_opengl.h>

#include "sdl-opengl.h"

int SDL_Surface_to_glTextureObject(SDL_Surface *suf, GLuint tex) {

	GLint	nOfColors;
	GLenum	texture_format;

	if(!suf) {
		return 1;
	}

	// Get the number of channels in the SDL surface
	nOfColors = suf->format->BytesPerPixel;
	if(nOfColors == 4) {
		if(suf->format->Rmask == 0x000000ff) {
			texture_format = GL_RGBA;
		} else {
			texture_format = GL_BGRA;
		}
	} else if(nOfColors == 3) {
		if(suf->format->Rmask == 0x000000ff) {
			texture_format = GL_RGB;
		} else {
			texture_format = GL_BGR;
		}
	} else {
		return 2;
	}

	// Bind the texture object
	glBindTexture(GL_TEXTURE_2D, tex);

	// Set the texture's stretching properties
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	glTexImage2D(GL_TEXTURE_2D, 0, nOfColors, suf->w, suf->h, 0,
			texture_format, GL_UNSIGNED_BYTE, suf->pixels);

	return 0;
}
