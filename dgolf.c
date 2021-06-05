//
// dgolf.c
// NESert Golfing, by Brad Smith 2019
// http://rainwarrior.ca
//

const char rom_version[] = " ################  "
	"NESert Golfing version 1.4 by Brad Smith, 2019"
	"  ################ ";

// for debugging performance
// (turns screen greyscale/green at end of frame when SELECT is held)
//#define PROFILE() { if (gamepad & PAD_SELECT) ppu_profile(0x41); }
//#define PROFILE() { ppu_profile(0x41); }
#define PROFILE() {}

// a few checks for safety, triggers infinite loop
#define ASSERT(__c__) {}
//#define ASSERT(__c__) { debug_assert(__c__); }

// for debugging graphics in left column
//#define SHOW_LEFT_COLUMN 1
#define SHOW_LEFT_COLUMN 0

// for testing the last hole
//#define LAST_HOLE_TEST 3
#define LAST_HOLE_TEST 0

// for debug hole skip with START button
//#define HOLE_SKIP 1
#define HOLE_SKIP 0

//
// common library stuff
//

typedef unsigned char      uint8;
typedef   signed char      sint8;
typedef unsigned short int uint16;
typedef   signed short int sint16;
typedef unsigned long  int uint32;
typedef   signed long  int sint32;

extern uint8* ptr;
extern uint32 seed; // 24-bit random seed, don't set to 0
extern uint8 i, j, k, l;
extern uint16 mx,nx,ox,px;
extern sint16 ux,vx;
#pragma zpsym("ptr")
#pragma zpsym("seed")
#pragma zpsym("i")
#pragma zpsym("j")
#pragma zpsym("k")
#pragma zpsym("l")
#pragma zpsym("mx")
#pragma zpsym("nx")
#pragma zpsym("ox")
#pragma zpsym("px")
#pragma zpsym("ux")
#pragma zpsym("vx")

extern uint8 input[16];
extern uint8 input_flags;
extern uint8 gamepad;
extern uint8 mouse1;
extern sint8 mouse2;
extern sint8 mouse3;
#pragma zpsym("input")
#pragma zpsym("input_flags")
#pragma zpsym("gamepad")
#pragma zpsym("mouse1")
#pragma zpsym("mouse2")
#pragma zpsym("mouse3")

extern uint8 prng(); // 8-bit random value
extern uint8 prng1(); // "fast" random value, only bit 0 is truly random (bits 1-7 have increasing entropy)
extern void mouse_sense(); // cycles sensitivity setting (doesn't work on Hyperkin clone)
extern void input_setup();
extern void input_poll();
extern void sound_play(const uint8* addr); // play a sound effect

extern void ppu_latch(uint16 addr); // write address to PPU latch
extern void ppu_direction(uint8 vertical); // set write increment direction
extern void ppu_write(uint8 value); // write value to $2007
extern void ppu_load(uint16 count); // uploads bytes from ptr to $2007 (clobbers ptr)
extern void ppu_fill(uint8 value, uint16 count); // uploads single value to $2007
extern void ppu_ctrl(uint8 v); // $2000, only bits 4-6 count (tile pages, sprite height), applies at next post
extern void ppu_mask(uint8 v); // $2001, applies at next post
extern void ppu_scroll_x(uint16 x);
extern void ppu_scroll_y(uint16 y);
extern void ppu_post(uint8 mode); // waits for next frame and posts PPU update
extern void ppu_profile(uint8 emphasis); // immediate $2001 write, OR with current mask (use bit 0 for greyscale)
extern void ppu_apply_direction(uint8 vertical); // immediately set write increment direction
extern void ppu_apply(); // immediately uploads ppu_send to $2007, resets ppu_send_count to 0

// POST_OFF     turn off rendering
// POST_NONE    turn on, no other updates
// POST_UPDATE  turn on, palette, send
// POST_DOUBLE  turn on, palette, send 64 bytes across 2 nametables
#define POST_OFF    1
#define POST_NONE   2
#define POST_UPDATE 3
#define POST_DOUBLE 4

#define PAD_A       0x80
#define PAD_B       0x40
#define PAD_SELECT  0x20
#define PAD_START   0x10
#define PAD_UP      0x08
#define PAD_DOWN    0x04
#define PAD_LEFT    0x02
#define PAD_RIGHT   0x01
#define MOUSE_L     0x40
#define MOUSE_R     0x80

extern uint16 ppu_send_addr;
extern uint8 ppu_send_count;
extern uint8 ppu_send[64];

extern uint8 palette[32];
extern uint8 oam[256];

//
// simple color blending library
//

extern uint8 blend50(uint8 a, uint8 b); // best palette match of 50% a, 50% b
extern uint8 blend25(uint8 a, uint8 b); // best palette match of 75% a, 25% b

//
// desert golfing stuff in assembly
//

extern uint8 layers_chr[];
extern uint8 sprite_chr[];
extern const uint16 LAYERS_CHR_SIZE;
extern const uint16 SPRITE_CHR_SIZE;

extern uint8 floor_column;
extern uint8 weather_tile;
extern uint8 weather_attribute;
extern sint8 weather_wind_dir;
extern uint8 weather_wind_p;
extern uint8 weather_rate_min;
extern uint8 weather_rate_mask;
extern uint8 hole;
extern uint8 ocean_attribute;
extern uint16 tx;
extern uint16 ty;
extern sint16 tsx;
extern sint16 tsy;
extern uint32 ball_x;
extern uint32 ball_y;
extern sint32 ball_vx;
extern sint32 ball_vy;
extern volatile uint32 balls_x[4]; // 16:8 fixed point + 8 bits of padding for convenience
extern volatile uint8 balls_fx[16]; // byte access to balls_x+0 (0,4,8,12), fixed 8
extern volatile uint8 balls_lx[15]; // byte access to balls_x+1 (0,4,8,12), low 8
extern volatile uint8 balls_hx[14]; // byte access to balls_x+2 (0,4,8,12), high 8
extern volatile uint16 balls_wx[7]; // word access to balls_x+1 (0,2,4,6), low,high 16
extern uint8  balls_y[4];
extern sint16 norm_x;
extern sint16 norm_y;
// note: while "volatile" is semantically important because of the type-punned aliases for balls_x
// it was actually a bad idea, because in cc65 it ends up disabling all optimizations in any function
// that uses volatile variables. In hindsight it would have been better to do this without the
// alias access.

#pragma zpsym("floor_column")
#pragma zpsym("weather_tile")
#pragma zpsym("weather_attribute")
#pragma zpsym("weather_wind_dir")
#pragma zpsym("weather_wind_p")
#pragma zpsym("weather_rate_min")
#pragma zpsym("weather_rate_mask")
#pragma zpsym("hole")
#pragma zpsym("ocean_attribute")
#pragma zpsym("tx")
#pragma zpsym("ty")
#pragma zpsym("tsx")
#pragma zpsym("tsy")
#pragma zpsym("ball_x")
#pragma zpsym("ball_y")
#pragma zpsym("ball_vx")
#pragma zpsym("ball_vy")
#pragma zpsym("balls_x")
#pragma zpsym("balls_fx")
#pragma zpsym("balls_lx")
#pragma zpsym("balls_hx")
#pragma zpsym("balls_wx")
#pragma zpsym("balls_y")
#pragma zpsym("norm_x")
#pragma zpsym("norm_y")

extern uint8 floor_render[64];
extern uint8 floor_y[512];
extern uint8 floor_a[512];

extern uint16 read_slope(uint8 index);
extern sint16 read_norm_x(uint8 index);
extern sint16 read_norm_y(uint8 index);
extern void floor_render_prepare(uint8 phase); // 0 = build, 1-5 = send, 6+ nothing (~6500, 300 x 4, 600, 0... cy)

extern void weather_animate(); // <~4300 cy
extern void weather_shift(uint8 shift); // ~800 cy

// fixed point multiply: (a * b) / 256
extern sint16 fmult(sint16 a, sint16 b); // ~545-594 cy
// used this to replace basis transforms like: ((a*b)+(c*d))/256
// (they are about 4x as fast, the original C code is left in comments for comparison)

//
// allocations and other common stuff
//

// fixed point for swing_x/y
#define SWING_FIX 16
// minimum radius for swing (to allow cancel)
#define SWING_MIN (SWING_FIX*3)
// maximum radius of swing
#define SWING_MAX (SWING_FIX*256)

// GLOBAL_FLOOR must be at least 2 pixels above OCEAN_FLOOR
// OCEAN_FLOOR must match definition in dgolf.s
#define GLOBAL_FLOOR 222
#define OCEAN_FLOOR 224
#define WATER_COLOUR 0x11
const uint8 BALL_COLOUR[5] = { 0x06, 0x02, 0x08, 0x0A, 0x00 };

// conventions:
// sx/sy = screen x,y (s = sprite)
// cx = circular x (mod 512)

#pragma bss-name (push, "ZEROPAGE") // was out of BSS space, but had plenty of ZEROPAGE

uint16 tee_cx;
uint16 hole_cx;
uint16 scroll_cx;

// render positions of things to draw

uint16 tee_sx;
uint16 hole_sx;
uint16 status_sx;

uint8 tee_sy;
uint8 hole_sy;

uint8 tee_s;
uint8 ball_s;
uint8 splash_s;

uint8 balls_draw[4]; // which player is in which draw-slot

uint8 hole_digits[3];
uint8 flag_remove;

uint8 players;
uint8 player;

uint8 swinging;
sint16 swing_x;
sint16 swing_y;

uint8 strokes[5*4];
uint8 cleared; // bit 0,1,2,3 = player in hole

uint8 status_w;
uint8 ring_glow;
uint8 roll_sx, roll_sy; // last frame's position
uint8 soll_sx, soll_sy; // two frames ago position
uint16 timeout; // frame timeout
uint8 rollout; // visual timeout (if ball is in same place for too long)
uint8 valley;
uint8 old_lip[2];
uint8 first_stroke;
uint8 next_player;

#pragma bss-name (pop)

// sprite building

uint8 oam_pos;

// floor generation

// control parameters
uint16 fg_min; // floor should not go below min
uint16 fg_max; // floor should not go below max
uint8 fg_hold_mask; // power of 2 - 1, controls maximum length of slopes
uint8 fg_angle_mask; // $80 | power of 2 - 1, lower values favour shallower slopes

uint16 fg_cx; // next floor pixel index to build
uint16 fg_last; // last floor value

uint16 fg_min_soft; // used to guide more toward centre
uint16 fg_max_soft;
uint16 fg_next; // next floor value
uint8 fg_angle; // last floor angle
uint8 fg_hold; // pixels to continue current angle

// animation of scenery etc.

uint8 frame_count;
uint8 transition;
uint8 transition_time;
uint8 day;
sint8 weather_wind_fade_dir;
uint8 weather_wind_fade_p;
uint16 weather_wind_timeout;

// hole generation

uint8 pal_floor;
uint8 pal_sky;
uint8 pal_text;

uint8 course;
uint8 field_set;

// title menu

uint8 title_menu;
uint8 gamepad_last;
uint8 gamepad_new;
uint8 mouse_last;
uint8 mouse_new;
uint8 mouse_steady;
sint8 mouse_sx;
sint8 mouse_sy;

//
// sound effects
//

// 0xFF = end
// 0xFE = all notes off
// 0xFD = next frame
// 0xXX 0xYY = write YY to register $4000+XX
// - start all sounds with $FE
// - end all sounds with $FE $FF

#define SFX_BEGIN  0xFE
#define SFX_END    0xFE, 0xFF
#define SFX_FRAME  0xFD
#define SFX(a_,b_) a_, b_

const uint8 SFX_STROKE[] = { SFX_BEGIN,
	SFX(0xC,0x3F), SFX(0xE,0x0C), SFX_FRAME,
	SFX(0xC,0x3D), SFX(0xE,0x00), SFX_FRAME,
	SFX(0xC,0x32),                SFX_FRAME,
	SFX(0xC,0x31),                SFX_FRAME, SFX_END };

const uint8 SFX_FLAG[] = { SFX_BEGIN,
	SFX(0x0,0xB2), SFX(0x2,0x93), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	SFX(0x0,0xB3), SFX(0x2,0x3F),                SFX_FRAME, SFX_FRAME,
	SFX(0x0,0xB4), SFX(0x2,0xEF), SFX(0x3,0x00), SFX_FRAME, SFX_FRAME,
	SFX(0x0,0xB5), SFX(0x2,0xBD),                SFX_FRAME, SFX_FRAME,
	SFX(0x0,0xB4), SFX(0x2,0x9F),                SFX_FRAME, SFX_FRAME,
	SFX(0x0,0xB3), SFX(0x2,0x7E),                SFX_FRAME, SFX_FRAME,
	SFX(0x0,0xB2), SFX(0x2,0x64),                SFX_FRAME, SFX_FRAME,
	SFX(0x0,0xB1), SFX(0x2,0x54),                SFX_FRAME, SFX_FRAME,
	SFX(0x0,0xB1), SFX(0x2,0x3F),                SFX_FRAME, SFX_FRAME, SFX_END };

const uint8 SFX_RESPAWN[] = { SFX_BEGIN,
	SFX(0x0,0x71), SFX(0x2,0x59), SFX_FRAME, SFX_FRAME,
	SFX(0x0,0x72), SFX(0x2,0x54), SFX_FRAME, SFX_FRAME,
	SFX(0x0,0x73), SFX(0x2,0x4F), SFX_FRAME, SFX_FRAME,
	SFX(0x0,0x74), SFX(0x2,0x4B), SFX_FRAME, SFX_FRAME,
	SFX(0x0,0x73), SFX(0x2,0x59), SFX_FRAME, SFX_FRAME,
	               SFX(0x2,0x54), SFX_FRAME, SFX_FRAME,
	               SFX(0x2,0x4F), SFX_FRAME, SFX_FRAME,
	               SFX(0x2,0x4B), SFX_FRAME, SFX_FRAME,
	SFX(0x0,0x71), SFX(0x2,0x59), SFX_FRAME, SFX_FRAME,
	               SFX(0x2,0x54), SFX_FRAME, SFX_FRAME,
	               SFX(0x2,0x4F), SFX_FRAME, SFX_FRAME,
	               SFX(0x2,0x4B), SFX_FRAME, SFX_FRAME, SFX_END };

const uint8 SFX_BOUNCE0[] = { SFX_BEGIN,
	SFX(0xC,0x31), SFX(0xE,0x0D), SFX_FRAME,
	SFX(0xC,0x32),                SFX_FRAME,
	SFX(0xC,0x31),                SFX_FRAME,
	SFX_FRAME, SFX_FRAME, SFX_END };

const uint8 SFX_BOUNCE1[] = { SFX_BEGIN,
	SFX(0xC,0x31), SFX(0xE,0x0E), SFX_FRAME,
	SFX(0xC,0x35),                SFX_FRAME,
	SFX(0xC,0x33),                SFX_FRAME,
	SFX(0xC,0x32),                SFX_FRAME,
	SFX(0xC,0x31),                SFX_FRAME, SFX_END };

const uint8 SFX_BOUNCE2[] = { SFX_BEGIN,
	SFX(0xC,0x32), SFX(0xE,0x0C), SFX_FRAME,
	SFX(0xC,0x35),                SFX_FRAME,
	SFX(0xC,0x34),                SFX_FRAME,
	SFX(0xC,0x32),                SFX_FRAME,
	SFX(0xC,0x31),                SFX_FRAME, SFX_END };

const uint8 SFX_SPLASH[] = { SFX_BEGIN,
	SFX(0xC,0x36), SFX(0xE,0x0F), SFX_FRAME,
	SFX(0xC,0x36), SFX(0xE,0x09), SFX_FRAME, SFX_FRAME, SFX_FRAME,
	SFX(0xC,0x38), SFX(0xE,0x06), SFX_FRAME,
	SFX(0xC,0x3C),                SFX_FRAME,
	SFX(0xC,0x39),                SFX_FRAME,
	SFX(0xC,0x38),                SFX_FRAME,
	SFX(0xC,0x37),                SFX_FRAME,
	SFX(0xC,0x36),                SFX_FRAME,
	SFX(0xC,0x35),                SFX_FRAME,
	SFX(0xC,0x34),                SFX_FRAME,
	SFX(0xC,0x33),                SFX_FRAME,
	SFX(0xC,0x32),                SFX_FRAME, SFX_FRAME, SFX_FRAME, SFX_FRAME,
	SFX(0xC,0x31),                SFX_FRAME, SFX_FRAME, SFX_FRAME, SFX_FRAME,
	SFX_FRAME, SFX_FRAME, SFX_END };

const uint8 SFX_TEE[] = { SFX_BEGIN,
	SFX(0x0,0xB2), SFX(0x2,0x9D), SFX(0x3,0x05), SFX_FRAME,
	               SFX(0x2,0xF9), SFX(0x3,0x02), SFX_FRAME,
	               SFX(0x2,0x9D), SFX(0x3,0x05), SFX_FRAME,
	               SFX(0x2,0x4C), SFX(0x3,0x05), SFX_FRAME,
	SFX(0x0,0xB3), SFX(0x2,0x80), SFX(0x3,0x02), SFX_FRAME,
	               SFX(0x2,0xB8), SFX(0x3,0x04), SFX_FRAME,
	               SFX(0x2,0x00), SFX(0x3,0x05), SFX_FRAME,
	               SFX(0x2,0x5C), SFX(0x3,0x02), SFX_FRAME,
	               SFX(0x2,0x74), SFX(0x3,0x04), SFX_FRAME,
	               SFX(0x2,0x34), SFX(0x3,0x04), SFX_FRAME,
	               SFX(0x2,0xFB), SFX(0x3,0x01), SFX_FRAME,
	               SFX(0x2,0xBF), SFX(0x3,0x03), SFX_FRAME,
	               SFX(0x2,0x89), SFX(0x3,0x03), SFX_FRAME,
	               SFX(0x2,0xAB), SFX(0x3,0x01), SFX_FRAME,
	               SFX(0x2,0x89), SFX(0x3,0x03), SFX_FRAME,
	               SFX(0x2,0xCE), SFX(0x3,0x02), SFX_FRAME,
	SFX(0x0,0xB1), SFX(0x2,0x89), SFX(0x3,0x03), SFX_FRAME,
	               SFX(0x2,0xAB), SFX(0x3,0x01), SFX_FRAME,
	               SFX(0x2,0x89), SFX(0x3,0x03), SFX_FRAME,
	               SFX(0x2,0xCE), SFX(0x3,0x02), SFX_FRAME, SFX_END };

const uint8 SFX_PROMPT0[] = { SFX_BEGIN,
	SFX(0x0, 0xB3), SFX(0x2,0xFB), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	                SFX(0x2,0xC4), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	                SFX(0x2,0x52), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	SFX(0x0, 0xB1),                               SFX_FRAME, SFX_FRAME,
	SFX_FRAME, SFX_FRAME, SFX_END };

const uint8 SFX_PROMPT1[] = { SFX_BEGIN,
	SFX(0x0, 0x73), SFX(0x2,0x7C), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	                SFX(0x2,0xAB), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	                SFX(0x2,0x3A), SFX(0x3,0x02), SFX_FRAME, SFX_FRAME,
	SFX(0x0, 0x71),                               SFX_FRAME, SFX_FRAME,
	SFX_FRAME, SFX_FRAME, SFX_END };

const uint8 SFX_PROMPT2[] = { SFX_BEGIN,
	SFX(0x0, 0xB3), SFX(0x2,0xAB), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	                SFX(0x2,0x52), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	                SFX(0x2,0x1C), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	SFX(0x0, 0xB1),                               SFX_FRAME, SFX_FRAME,
	SFX_FRAME, SFX_FRAME, SFX_END };

const uint8 SFX_PROMPT3[] = { SFX_BEGIN,
	SFX(0x0, 0x33), SFX(0x2,0xFD), SFX(0x3,0x00), SFX_FRAME, SFX_FRAME,
	                SFX(0x2,0x2D), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	                SFX(0x2,0x7C), SFX(0x3,0x01), SFX_FRAME, SFX_FRAME,
	SFX(0x0, 0x31),                               SFX_FRAME, SFX_FRAME,
	SFX_FRAME, SFX_FRAME, SFX_END };

const uint8* const SFX_PROMPTS[4] = {
	SFX_PROMPT0,
	SFX_PROMPT1,
	SFX_PROMPT2,
	SFX_PROMPT3,
};

//
// misc
//

void debug_assert(int v)
{
	if (!v)
	{
		ppu_profile(0x81); // red tinted grey
		while(1); // infinite loop
	}
}

uint16 mag_squared_s8(sint8 x, sint8 y)
{
	if (x < 0) x = -x;
	if (y < 0) y = -y;
	return ((uint16)x * x) + ((uint16)y * y);
}

//
// graphical stuff
//

void palette_generate(uint8 sky, uint8 ground, uint8 text)
{
	palette[ 0] = palette[16] =
	palette[ 2] = palette[10] =
	palette[ 5] = palette[13] = sky;
	palette[26] =
	palette[ 9] = palette[11] =
	palette[14] = palette[15] = ground;
	palette[ 1] = palette[ 3] =
	palette[ 6] = palette[ 7] = text;
	//palette[26] = 0x21; // for debugging tee
	
	if (ocean_attribute != 255)
	{
		palette[3] = WATER_COLOUR;
	}
}

void ball_draw_setup()
{
	// colour for current player
	i = BALL_COLOUR[player];
	palette[17] = i | 0x10;
	palette[18] = i | 0x20;
	palette[19] = i | 0x30;

	// 3 remaining players packed into one palette
	balls_draw[0] = player;
	for (i=1; i<4; ++i)
	{
		if (i >= players) balls_draw[i] = 4; // hidden
		else balls_draw[i] = (player + i) % players;
	}

	for (i=1; i<4; ++i)
	{
		palette[20+i] = BALL_COLOUR[balls_draw[i]] | 0x10;
	}
}

uint8 attribute(tl,tr,bl,br)
{
	return tl|(tr<<2)|(bl<<4)|(br<<6);
}

void ppu_text(const char* text, uint16 addr)
{
	ptr = (uint8*)text;
	nx = addr;
	ppu_latch(nx);
	i = *ptr;
	while (i)
	{
		if (i == '\n')
		{
			nx += 32;
			ppu_latch(nx);
		}
		else ppu_write(i);
		++ptr;
		i = *ptr;
	}
}

void cls() // erase nametables
{
	ppu_latch(0x2000);
	ppu_fill(0x00,0x1000);
}

void sprite_begin()
{
	oam_pos = 4;
	// sprite 0 always untouched
}

void sprite_end()
{
	//while (oam_pos != 0)
	while (oam_pos < (32*4)) // leaves last 32 for weather
	{
		oam[oam_pos] = 0xFF;
		oam_pos += 4;
	}
}

void sprite_add(uint8 tile, uint8 x, uint8 y, uint8 attrib)
{
	(oam+2)[oam_pos] = attrib;
	(oam+0)[oam_pos] = (uint8)(y-1);
	(oam+3)[oam_pos] = x;
	(oam+1)[oam_pos] = tile;
	oam_pos += 4;
}

//
// floor generation
//

void floor_build_pixel_calculate_range()
{
	fg_min_soft = (fg_min/4) + (fg_max/4) + (fg_min/2);
	fg_max_soft = (fg_min/4) + (fg_max/4) + (fg_max/2);
}

void floor_build_pixel_angle()
{
	fg_hold = (prng() & fg_hold_mask) + 8;
	fg_angle = prng() & fg_angle_mask;

	// drive toward centre
	if      (fg_last < fg_min_soft) fg_angle &= 0x7F;
	else if (fg_last > fg_max_soft) fg_angle |= 0x80;
}

void floor_build_pixel_next()
{
	fg_next = fg_last + read_slope(fg_angle);
}

const uint8 HOLE_ANGLE[8] = { 4, 4, 0, 0, 0x80+0, 0x80+0, 0x80+4, 0x80+4 };

// builds one pixel worth of floor
// note that this should always be 8 pixels ahead of floor_column,
// so that floor_render_prepare has 8 pixels worth of stuff to build,
// (and floor_column should be another 8 pixels ahead of the scroll)
void floor_build_pixel()
{
	uint16 h;

	if (ocean_attribute != 255) goto build;

	h = fg_cx - hole_cx;
	if (h < 8)
	{
		if (hole == 0)
		{
			ocean_attribute = ((floor_column + 1) / 4) & 15;
			fg_last = fg_next = OCEAN_FLOOR * 256;
			fg_angle = 0;
			palette[3] = WATER_COLOUR;
			goto build;
		}
		
		fg_hold = 0;
		fg_angle = HOLE_ANGLE[(uint8)h];
		floor_build_pixel_next();
		goto build;
	}

	if (fg_hold) --fg_hold;
	else floor_build_pixel_angle();
	
	floor_build_pixel_next();
	if (fg_next < fg_min)
	{
		floor_build_pixel_angle();
		fg_angle &= 0x7F; // force positive
		floor_build_pixel_next();
	}
	else if (fg_next > fg_max)
	{
		floor_build_pixel_angle();
		fg_angle |= 0x80; // force negative
		floor_build_pixel_next();
	}

build:
	floor_y[fg_cx] = ((fg_last/2) + (fg_next/2)) / 256;
	floor_a[fg_cx] = fg_angle;

	fg_last = fg_next;
	fg_cx = (fg_cx + 1) & 511;
}

//
// hole generation
//

//                        snow  rain  brwn  pink  dbrn  gren  purp  yell, night...
const uint8 set_f[16] = { 0x30, 0x1A, 0x17, 0x25, 0x07, 0x19, 0x13, 0x27, 0x30, 0x1A, 0x17, 0x15, 0x07, 0x19, 0x13, 0x37 };
const uint8 set_s[16] = { 0x21, 0x2B, 0x27, 0x35, 0x17, 0x21, 0x23, 0x37, 0x0F, 0x0B, 0x0F, 0x05, 0x0F, 0x09, 0x03, 0x01 };
const uint8 set_t[16] = { 0x0F, 0x30, 0x0F, 0x0F, 0x30, 0x0F, 0x0F, 0x0F, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30 };
const uint8 set_w[16] = { 0x39, 0x38, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x39, 0x38, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

// 18 hole repeating course terrain structure

// angle mask:
// 0 artificial angular, mostly flat
// 1 favour moderate slopes
// 2 all slopes
#define AM0 0x9F
#define AM1 0xBF
#define AM2 0xFF

// hold mask:
// 0 short segments
// 1 medium segments
// 2 long segments
#define HM0 15
#define HM1 31
#define HM2 63

const uint8 course_hm[18] = { HM1,HM0,HM1,HM0,HM1,HM1,HM2,HM1,HM1, HM1,HM1,HM2,HM1,HM0,HM2,HM1,HM2,HM1 };
const uint8 course_am[18] = { AM0,AM0,AM1,AM1,AM1,AM2,AM1,AM1,AM2, AM1,AM2,AM1,AM2,AM2,AM2,AM1,AM2,AM1 };

void hole_next()
{
	++hole;
	for (i=0;i<3;++i)
	{
		++hole_digits[i];
		if (hole_digits[i] < 10) break;
		hole_digits[i] = 0;
	}
	
	#if LAST_HOLE_TEST
		if (hole == LAST_HOLE_TEST) hole = 0;
	#endif

	fg_hold_mask = course_hm[course];
	fg_angle_mask = course_hm[course];
	++course; if (course >= 18) course = 0;

	mx = hole_cx - scroll_cx;
	if (hole_cx < scroll_cx) mx += 512;
	i = (mx / 8) + 7; // min screen column of next hole
	j = i + 13; // max screen column of next hole
	if (!hole) j = i+1; // "last hole" should be close
	if (i < 34) i = 34; // must be at least 2 tiles offscreen
	k = j - i; // allowed range
	if (k == 0 || k >= 16) k = 1; // prevent invalid ranges (shouldn't occur?)
	l = i + (prng() % k); // new hole column

	ASSERT((scroll_cx & 7) == 0);
	if (!hole)
	{
		// final hole must fall on 32-pixel attribute boundary
		while (((scroll_cx + (l*8)) & 24) != 0) ++l;
		// force terrain to go down toward ocean
		fg_min = (GLOBAL_FLOOR - 8) * 256;
		floor_build_pixel_calculate_range();
	}

	tee_cx = hole_cx; // last hole becomes new tee
	hole_cx = (scroll_cx + (l * 8)) & 511; // place new hole
	hole_sx = l * 8; // screen position of new hole

	if (!hole) hole_sx = 2048; // place it offscreen for the ocean
}

//
// common animation
//

#define TRANSITION_TIME 16
#define DAY_TIME 8

void transition_animate()
{
	uint8 s, f;

	--transition;
	if (transition == ((TRANSITION_TIME*3)/4))
	{
		s = blend25(pal_sky,   set_s[field_set]);
		f = blend25(pal_floor, set_f[field_set]);
		palette_generate(s,f,s);
	}
	else if (transition == ((TRANSITION_TIME*2)/4))
	{
		s = blend50(pal_sky,   set_s[field_set]);
		f = blend50(pal_floor, set_f[field_set]);
		palette_generate(s,f,s);
	}
	else if (transition == ((TRANSITION_TIME*1)/4))
	{
		s = blend25(set_s[field_set], pal_sky);
		f = blend25(set_f[field_set], pal_floor);
		palette_generate(s,f,s);
	}
	else if (transition == 0)
	{
		pal_sky   = set_s[field_set];
		pal_floor = set_f[field_set];
		palette_generate(pal_sky, pal_floor, pal_sky);
	}
}

void weather_fade_automask()
{
	// scale back the randomness at each power of 2
	if ((weather_rate_min & (weather_rate_min-1)) == 0)
	{
		weather_rate_mask = weather_rate_min-1;
	}
}

void weather_wind_random()
{
	weather_wind_fade_dir = 1 - ((prng1() & 1) * 2); // 1 or -1, wind direction
	weather_wind_fade_p = prng(); // wind strength
	weather_wind_timeout = ((prng() & 15) + 32) * 64; // 30-50 seconds before wind changes
}

void weather_fade()
{
	if (weather_wind_timeout == 0)
		weather_wind_random();
	else
		--weather_wind_timeout;
	
	// smoothly change wind
	if (weather_wind_dir != weather_wind_fade_dir)
	{
		if (weather_wind_p == 0) weather_wind_dir = weather_wind_fade_dir;
		else --weather_wind_p;
	}
	else
	{
		if      (weather_wind_p < weather_wind_fade_p) ++weather_wind_p;
		else if (weather_wind_p > weather_wind_fade_p) --weather_wind_p;
	}

	// smoothly change particles drop rate

	if (frame_count & 15) return;

	if (set_w[field_set])
	{
		if (weather_rate_min == 0)
		{
			weather_rate_min = 64;
			weather_fade_automask();
		}
		else if (weather_rate_min > 4)
		{
			--weather_rate_min;
			weather_fade_automask();
		}
	}
	else
	{
		if (weather_rate_min != 0)
		{
			++weather_rate_min;
			weather_fade_automask();
			if (weather_rate_min >= 65) weather_rate_min = 0;
		}
	}
}

void weather_attribute_set()
{
	// sets speed and palette via whether tile is rain or snow
	// the unused bits of the OAM attribute byte are used to control particle fall speed
	weather_attribute = (weather_tile == 0x38) ? ((4<<2)|3) : ((1<<2)|2);
}

void flag_animate()
{
	uint8 t;
	
	if (flag_remove == 0) return;
	t = hole_sy - 32;
	if (t > hole_sy)
	{
		hole_sy = 255;
		flag_remove = 0;
		return;
	}
	hole_sy = t;
}

void frame()
{
	if (transition) transition_animate();
	weather_fade();
	weather_animate();
	flag_animate();
	PROFILE();
	ppu_post(POST_UPDATE);
	++frame_count;
}

void frame_double()
{
	weather_fade();
	weather_animate();
	flag_animate();
	PROFILE();
	ppu_post(POST_DOUBLE);
	++frame_count;
}

void frames(uint8 count)
{
	while (count--) frame();
}

void delay(uint8 frames)
{
	while (frames--) frame();
}

//
// main menu
//

const char help_text[] =
	"       NESert Golfing\n"
	"      Brad Smith, 2019\n"
	"   http://rainwarrior.ca\n"
	"\n"
	"GAMEPAD CONTROL:\n"
	"  Hold A to begin swing,\n"
	"  use directions to aim,\n"
	"  release to stroke.\n"
	"  Hold B for fine control.\n"
	"\n"
	"MOUSE CONTROL:\n"
	"  Hold left button to begin\n"
	"  swing, drag to aim,\n"
	"  release to stroke.\n"
	"  Hold right button for\n"
	"  fine control.\n"
	"\n"
	" NESert Golfing was derived\n"
	"   from the original game\n"
	"       Desert Golfing\n"
	"   by Justin Smith, 2014.";

const char title_text[] =
	"         Play\n"
	"\n"
	"How many?   1  2  3  4\n"
	"\n"
	"         Help";

#define MOUSE_STEADY_TIME 24
#define MOUSE_STEADY_MOVE 12

void new_poll()
{
	input_poll();
	gamepad_new = gamepad & (gamepad ^ gamepad_last);
	mouse_new = mouse1 & (mouse1 ^ mouse_last);
	gamepad_last = gamepad;
	mouse_last = mouse1;

	// filter out new mouse motions until it's been stationary for a few frames
	mouse_sx = 0;
	mouse_sy = 0;
	if (mouse_steady)
	{
		--mouse_steady;
	}
	else
	{
		if (mouse3 >= MOUSE_STEADY_MOVE || mouse3 <= -MOUSE_STEADY_MOVE)
		{
			mouse_sx = mouse3;
			mouse_steady = MOUSE_STEADY_TIME;
		}
		if (mouse2 >= MOUSE_STEADY_MOVE || mouse2 <= -MOUSE_STEADY_MOVE )
		{
			mouse_sy = mouse2;
			mouse_steady = MOUSE_STEADY_TIME;
		}
	}
}

void help()
{
	#define HELP_SCROLL_RATE 16

	sprite_begin();
	sprite_end();

	// scroll in
	nx = 512;
	for (i = 0; i < (256 / HELP_SCROLL_RATE); ++i)
	{
		nx -= HELP_SCROLL_RATE;
		ppu_scroll_x(nx);
		weather_shift(HELP_SCROLL_RATE);
		frame();
		new_poll();
	}
	
	while (1) // wait for button
	{
		ppu_scroll_x(256);
		frame();
		new_poll();
		if (gamepad_new || mouse_new) break;
	}
	
	// scroll out
	nx = 256;
	for (i=0; i < (256 / HELP_SCROLL_RATE); ++i)
	{
		nx += HELP_SCROLL_RATE;
		ppu_scroll_x(nx);
		weather_shift(-HELP_SCROLL_RATE);
		frame();
		new_poll();
	}

	return;
}

void title()
{
	cls();
	
	palette[25] = 0x2D; // grey tee / flagpole
	//      26          // floor (palette_generate)
	palette[27] = 0x30; // white snow
	palette[29] = 0x00; // unused
	palette[30] = 0x00; // unused
	palette[31] = WATER_COLOUR; // rain

	gamepad_last = gamepad;
	mouse_last = mouse1;
	mouse_steady = 0;
	mouse_sx = 0;
	mouse_sy = 0;

	players = 1;
	frame_count = 0;
	ocean_attribute = 255;
	flag_remove = 0;
	course = 0;
	field_set = prng() & 15;
	pal_floor    = set_f[field_set];
	pal_sky      = set_s[field_set];
	pal_text     = set_t[field_set];
	weather_tile = set_w[field_set];
	weather_rate_mask = 3;
	weather_rate_min = weather_tile ? 4 : 0;
	weather_wind_random();
	weather_wind_p = weather_wind_fade_p;
	weather_wind_dir = weather_wind_fade_dir;
	weather_attribute_set();
	transition = 0;
	transition_time = (prng() & 7) + 3;
	day = prng() & 7;

	// keep hole and start off the field for menu
	hole_cx = 512;
	tee_cx = 512;

	// build initial floor
	fg_cx = 256;
	fg_max = GLOBAL_FLOOR * 256; // global maximum
	fg_min = (192+3) * 256; // 3 pixels below help text
	fg_angle_mask = 0x80 | 0x3F; // shallow hills
	floor_build_pixel_calculate_range();
	fg_hold_mask = 7; // short hills
	fg_last = fg_min + (prng() % (fg_max - fg_min));
	fg_hold = 0;
	for (mx = 0; mx < 256; ++mx) floor_build_pixel();
	fg_min = (176+3) * 256; // 3 pixels below menu
	floor_build_pixel_calculate_range();
	for (mx = 0; mx < 256; ++mx) floor_build_pixel();
	fg_min = 48 * 256; // global minimum
	floor_build_pixel_calculate_range();

	// render initial floor
	floor_column = 32;
	for (mx = 0; mx < 64; ++mx)
	{
		for (j=0;j<6;++j)
		{
			floor_render_prepare(j);
			if (j>0) ppu_apply();
		}
	}
	ppu_apply_direction(0);

	// title
	i = 16;
	for (j=0; j<7; ++j)
	{
		ppu_latch(0x2000 + 8 + ((4+j) * 32));
		for (k=0; k<16; ++k) { ppu_write(i); ++i; }
	}
	ppu_latch(0x2000 + (8+13) + ((4+7) * 32));
	ppu_write(13);
	ppu_write(14);
	ppu_write(15);

	// menu
	ppu_latch(0x23C0 + 0 + (12*2));
	ppu_fill(attribute(1,1,1,1),16);
	ppu_fill(attribute(2,2,2,2),24);
	ppu_text(title_text, 0x2000 + 5 + (15 * 32));

	// help
	ppu_latch(0x27C0 + 0 + (0*2));
	ppu_fill(attribute(1,1,1,1),48);
	ppu_fill(attribute(3,3,3,3),16);
	ppu_text(help_text, 0x2400 + 2 + (3 * 32));

	palette_generate(pal_sky, pal_floor, pal_text);

	title_menu = 0;
	ppu_scroll_x(0);
	ppu_scroll_y(0);
	palette[18] = 0x24;

	while (1)
	{
		#define MENU_Y0 (15*8)
		#define MENU_Y1 (17*8)
		#define MENU_Y2 (19*8)
		#define MENU_X0 (12*8)
		#define MENU_X1 (19*8)
		#define MENU_XP1 ((16-3)*8)

		palette[22] = BALL_COLOUR[players-1];
		i = (frame_count / 8) & 3;
		if (i == 0) i = 2;
		palette[22] |= i << 4;

		sprite_begin();
		if (title_menu != 1)
		{
			uint8 i = title_menu==0 ? MENU_Y0 : MENU_Y2;
			sprite_add(0x3A, MENU_X0,i,0x00);
			sprite_add(0x3A, MENU_X1,i,0x40);
			palette[22] = (palette[22] & 0x0F) | 0x10; // darken, not selected
		}
		sprite_add(0x3A, MENU_XP1+(players*24), MENU_Y1, 0x01);
		sprite_add(0x3A, MENU_XP1+16+(players*24), MENU_Y1, 0x41);
		sprite_end();

		frame();
		new_poll();

		if ((mouse_new & MOUSE_L) || (gamepad_new & PAD_START)) // START or LMB starts game unless help is selected
		{
			if (title_menu != 2) break;
			else help();
		}
		else if (mouse_new & MOUSE_R) // RMB cycles players
		{
			players = (players & 3) + 1;
		}
		else if ((title_menu != 1) && (gamepad_new & (PAD_A | PAD_B))) // A/B can also start game or help
		{
			if (title_menu != 2) break;
			else help();
		}
		else if ((gamepad_new & (PAD_DOWN | PAD_SELECT)) || mouse_sy > 0) // SELECT or down on pad/mouse to switch selection
		{
			++title_menu;
			if (title_menu > 2) title_menu = 0;
		}
		else if ((gamepad_new & PAD_UP) || mouse_sy < 0) // up on pad/mouse to switch selection (reverse)
		{
			--title_menu;
			if (title_menu > 2) title_menu = 2;
		}
		else if (title_menu == 1) // players selection
		{
			if ((gamepad_new & (PAD_A | PAD_RIGHT)) || mouse_sx > 0) // A or right on pad/mouse to increase players
			{
				players = (players & 3) + 1;
			}
			else if ((gamepad_new & PAD_LEFT) || mouse_sx < 0) // left on pad/mouse to decrease players
			{
				players = ((players + 2) & 3) + 1;
			}
		}

		prng(); // build up entropy

		// colour cycle the cursor
		if (!(frame_count & 7))
		{
			i = (palette[18] & 0x0F) + 1;
			if (i >= 0xD) i = 1;
			palette[18] = 0x20 | i;
		}
	}

	// prepare for game

	// clear sprites
	sprite_begin();
	sprite_end();

	// fade out text
	i = pal_text;
	palette[1] = palette[3] = palette[6] = palette[7] = blend25(i,pal_sky); frames(2);
	palette[1] = palette[3] = palette[6] = palette[7] = blend50(i,pal_sky); frames(2);
	palette[1] = palette[3] = palette[6] = palette[7] = blend25(pal_sky,i); frames(2);
	palette[1] = palette[3] = palette[6] = palette[7] = pal_sky;

	// wipe existing text
	for (i=0; i<64; ++i) ppu_send[i] = 0;
	for (i=3; i<20; ++i)
	{
		ppu_send_addr = 0x2000 + (32 * i);
		frame_double();
	}
	for (i=20; i<24; ++i)
	{
		ppu_send_addr = 0x2400 + (32 * i);
		ppu_send_count = 32;
		frame();
	}
	
	// replace attributes
	j = attribute(1,1,1,1); for (i=0; i< 8; ++i) ppu_send[i] = j;
	j = attribute(2,2,2,2); for (i=8; i<64; ++i) ppu_send[i] = j;
	ppu_send_addr = 0x23C0;
	ppu_send_count = 64;
	frame();
	j = attribute(3,3,3,3); for (i=8; i<64; ++i) ppu_send[i] = j;
	ppu_send_addr = 0x27C0;
	ppu_send_count = 64;
	frame();

	// create first tee (flat ground)
	hole_cx = 256;
	tee_sx = 256;
	tee_sy = fg_last / 256;
	tee_s = 7;
	for (i=0; i<8; ++i)
	{
		floor_y[fg_cx+i] = tee_sy;
		floor_a[fg_cx+i] = 0;
	}
	fg_cx += 8;
	scroll_cx = 0;
	hole = 0;
	hole_digits[0] = hole_digits[1] = hole_digits[2] = 0;
	hole_next(); // hole_cx converts to tee_cx, new hole_cx is created
	
	// render the tee, and keep the floor build 8 pixels ahead of it
	for (i=0; i<8; ++i)
	{
		floor_build_pixel();
		floor_render_prepare(i);
		frame();
	}

	// place players on tee, reset other variables
	status_sx = 256;
	balls_x[0] =
	balls_x[1] =
	balls_x[2] =
	balls_x[3] = 256 * 256;
	balls_y[0] =
	balls_y[1] =
	balls_y[2] =
	balls_y[3] = tee_sy - 6;
	
	ball_s = 0;
	splash_s = 0;
	player = 0;
	next_player = 0;
	swinging = 0;
	status_w = 0;
	ring_glow = 0;
	first_stroke = (players > 1) ? 0 : 1;
	status_w = (16 - (((7 * players) - 2) / 2)); // position to start the strokes display
	ball_draw_setup();

	return; // hole_play()
}

//
// play loop
//

void hole_shift()
{
	--tee_sx;
	--hole_sx;
	balls_wx[0] -= 1;
	balls_wx[2] -= 1;
	balls_wx[4] -= 1;
	balls_wx[6] -= 1;
}

void hole_draw_flag()
{
	if (hole_sy == 255) return;

	j = (uint8)hole_sx;
	i = j + 2;
	if (i < j) return;

	sprite_add(0x3C, i, hole_sy- 1, 0x02); // flagpole bottom
	sprite_add(0x2C, i, hole_sy- 9, 0x02); // flagpole mid
	sprite_add(0x1C, i, hole_sy-17, 0x02); // flagpole top

	i += 8;
	k = hole_sy-16;
	l = hole_sy-24;
	if (i < j) return;

	if (hole >= 100)
	{
		sprite_add(0x10 | hole_digits[2], i, k, 0x02);
		sprite_add(0x1B                 , i, l, 0x02);
		i += 8;
		if (i < j) return;
	}

	if (hole >= 10)
	{
		sprite_add(0x10 | hole_digits[1], i, k, 0x02);
		sprite_add(0x1B                 , i, l, 0x02);
		i += 8;
		if (i < j) return;
	}

	sprite_add(0x10 | hole_digits[0], i, k, 0x02); // numeral
	sprite_add(0x1B                 , i, l, 0x02); // top line
	i += 8;
	if (i < j) return;

	sprite_add(0x1A, i, k, 0x02); // flag tip
}

const uint8 RING_CYCLE[4] = { 0x00, 0x80, 0xC0, 0x40 }; // rotating ring for sprite 3B
const uint8 RING_OFFX [4] = {    0,    0,    1,    1 };
const uint8 RING_OFFY [4] = {    0,    1,    1,    0 };
const uint8 RING_GLOW [4] = { 0x3D, 0x3E, 0x3F, 0x3F }; // glowing sprites for motion indicator

// clobbers i,j,k,l,mx,nx,ox,px
void hole_draw()
{
	static uint8 order; // needed internal value because i,j,k,l are clobbered by hole_draw_flag

	ox = balls_wx[player*2];
	px = balls_y[player];

	sprite_begin();
	if (swinging)
	{
		sprite_add(0x2A, ox, px, 0x00); // highlighted ball

		if (
			swing_x >=  SWING_MIN ||
			swing_x <= -SWING_MIN ||
			swing_y >=  SWING_MIN ||
			swing_y <= -SWING_MIN )
		{
			// note: the +1/2 on swing offsets is a rounding adjustment to keep negative/positive visually symmetrical
			tsx = (swing_x + (SWING_FIX/2)) / SWING_FIX;
			tsy = (swing_y + (SWING_FIX/2)) / SWING_FIX;

			// rotating ring in direction of shot
			order = ((swing_x + swing_y) / (SWING_FIX / 4)) & 3;
			tx = ox - tsx - RING_OFFX[order];
			ty = px - tsy - RING_OFFY[order];
			if (tx < 256 && ty < 256) sprite_add(0x2B, (uint8)tx, (uint8)ty, RING_CYCLE[order]);

			// solid dark ring direction of pull-back for swing
			tx = ox + tsx;
			ty = px + tsy;
			if (tx < 256 && ty < 256) sprite_add(0x29, (uint8)tx, (uint8)ty, 0x00);

			// separated from frame count so it's not synchronized with it
			// (want to avoid some bad colour against the background always
			// appearing in the same phase in this visualization of direction)
			ring_glow += 13;

			#define SWING_FRAMES (1<<4)
			order = frame_count & (SWING_FRAMES-1);
			mx = (tsx * (sint16)order) / (SWING_FRAMES-1);
			nx = (tsy * (sint16)order) / (SWING_FRAMES-1);
			tx = ox - mx;
			ty = px - nx;
			if (tx < 256 && ty < 256) sprite_add(RING_GLOW[(ring_glow/32)&3], (uint8)tx, (uint8)ty, 0x00);

			tx += tsx;
			ty += tsy;
			if (tx < 256 && ty < 256) sprite_add(RING_GLOW[(ring_glow/32)&3], (uint8)tx, (uint8)ty, 0x00);
		}
	}
	else if (ox < 256)
	{
		if (splash_s) // ball splashing into water
		{
			sprite_add(0x1F + splash_s, ox, px, 0x03);
			if (splash_s < 4) // ball behind splash
				sprite_add(0x1D + (ball_s/2), ox, px + splash_s - 1, 0x00);
		}
		else // ball by itself
			sprite_add(0x1D + (ball_s/2), ox, px, 0x00);
	}
	if (tee_sx < 256) sprite_add(0x30 | tee_s, (uint8)tee_sx, tee_sy, 0x02);
	if (status_sx < 256) sprite_add(0x3A, (uint8)status_sx, 16, 0x00);
	
	// cycling order of remaining sprites
	for (order=4; order!=0; --order)
	{
		i = (frame_count + order) & 3;
		switch(i)
		{
			case 0:
				if (hole_sx < 256) hole_draw_flag();
				break;
			case 1:
			case 2:
			case 3:
				j = balls_draw[i];
				k = j * 4;
				if (j < players && !balls_hx[k])
					sprite_add(0x2C + i, balls_lx[k], balls_y[j], 0x01);
				break;
		}
	}
	sprite_end();
}

void stroke_add()
{
	i = player * 5;
	for (j=0; j<5; ++j)
	{
		++strokes[i];
		if(strokes[i] < 10) return;
		strokes[i] = 0;
		++i;
	}
	// maxed out
	i -= 5;
	strokes[i] =
	strokes[i+1] =
	strokes[i+2] =
	strokes[i+3] =
	strokes[i+4] = 9;
}

void status_draw()
{
	uint8 ws;
	uint8 w;

	for (i=0; i<64; ++i) ppu_send[i] = 0; // blank the line
	
	ASSERT((scroll_cx & 7) == 0);
	w = (status_w + (scroll_cx / 8)) & 63; // adjust for scroll

	ppu_send_addr = 0x2000 + (2 * 32);

	// draw stroke counter for each player
	k = 4;
	for (i=0; i<players; ++i)
	{
		ws = w;
		for (j=0; j<4; ++j) // left justify by skipping leading 0s
		{
			if (strokes[k] != 0) break;
			--k;
		}
		for ( ; j<5; ++j)
		{
			ppu_send[ws] = 0x30 | strokes[k];
			--k;
			ws = (ws + 1) & 63;
		}
		k += 10;
		w = (w + 7) & 63;
	}
}

void status_player()
{
	if (players > 1)
		status_sx = ((status_w + (player * 7)) - 1) * 8; // player indicator
}

#define status_player_hide() { status_sx = 256; }

void hole_splash()
{
	uint8 t;
	sound_play(SFX_SPLASH);
	for (t=0; t<18; ++t)
	{
		splash_s = (t/2)+1;
		balls_y[player] = OCEAN_FLOOR-6;
		hole_draw();
		frame();
	}
}

// collision priority per-pixel overlapping the bottom half of the ball
// favours closeness to the centre, and right side before left
// (lowest wins, 13=miss)
const uint8 COLLIDE_PRIORITY[5*4] = {
	 8, 12, 13, 13, // left column
	 3,  5, 11, 13,
	 0,  1,  6, 13, // centre column
	 2,  4,  9, 13,
	 7, 10, 13, 13, // right column
};

void status_bar_fade_in()
{
	pal_text = set_t[field_set];
	status_draw();
	hole_draw(); frame_double();
	palette_generate(pal_sky, pal_floor, blend25(pal_sky, pal_text)); hole_draw(); frames(2);
	palette_generate(pal_sky, pal_floor, blend50(pal_sky, pal_text)); hole_draw(); frames(2);
	palette_generate(pal_sky, pal_floor, blend25(pal_text, pal_sky)); hole_draw(); frame();
	input_poll(); hole_draw(); frame(); // clear pending input
	palette_generate(pal_sky, pal_floor, pal_text);
}

void hole_play()
{
	uint16 t = (tee_cx - (6*8)) & 511; // target scroll position
	while (scroll_cx != t)
	{
		// build one more pixel of floor
		floor_build_pixel();
		floor_render_prepare(scroll_cx & 7);

		// scroll one pixel to the right
		weather_shift(-1);
		hole_shift();
		scroll_cx = (scroll_cx + 1) & 511;
		ppu_scroll_x(scroll_cx);
		
		if (hole_sx < 256) hole_sy = floor_y[hole_cx + 4] - 8;
		
		hole_draw();
		frame();
	}
	
	// 3. raise the tee
	if (hole != 1)
	{
		sound_play(SFX_TEE);
		tee_sy = floor_y[tee_cx + 4] - 8;
		tee_sx = (tee_cx - scroll_cx) & 511;
		for (t = 0; t < 8; ++t)
		{
			tee_s = t;
			i = tee_sy + 1 - t;
			for (j=0; j<4; ++j)
				if (balls_y[j] > i) balls_y[j] = i;
			hole_draw();
			frames(2);
		}
		for (i = 0; i < 8; ++i)
		{
			floor_a[tee_cx+i] = 0; // flat
			floor_y[tee_cx+i] = tee_sy;
		}
		// restore the lip
		floor_a[(tee_cx-1)&511] = old_lip[0];
		floor_a[(tee_cx+8)&511] = old_lip[1];
	}
	// add a little "lip" for the hole to help balls fall in
	if (hole != 0)
	{
		old_lip[0] = floor_a[(hole_cx-1)&511];
		old_lip[1] = floor_a[(hole_cx+8)&511];
		floor_a[(hole_cx-1)&511] = 0x01;
		floor_a[(hole_cx+8)&511] = 0x81;
	}

	while (transition) { hole_draw(); frame(); } // finish any pending colour transition

	// 4. fade in status bar
	if (!first_stroke) // delay this on first stroke in 1-player mode
		status_bar_fade_in();

	// 5. play hole

	// find leading player (after the one who played first last)
	for (t=1; t<4; ++t)
	{
		j = (next_player + t) & 3;
		if (j >= players) continue;
		for (i = 0; i<5; ++i)
		{
			k = strokes[(j*5)+4-i];
			l = strokes[(next_player)*5+4-i];
			if (k > l) break;
			if (k < l)
			{
				next_player = j;
				break;
			}
		}
	}
	player = next_player;
	next_player = (next_player + 1) % players; // favour the next one if tied to keep it cycling

	cleared = 0;
	for (i=0; i<4; ++i)
		if (i >= players) cleared |= (1 << i);
	ball_draw_setup();

	// play sound to indicate current player
	if (players > 1)
	{
		hole_draw(); frames(2);
		sound_play(SFX_PROMPTS[player]);
	}


	while (cleared < 0x10)
	{
		// if centre of ball is offscreen, put it back on the tee
		if (balls_x[player] >= (253*256) || balls_x[player] < (5*256))
		{
			if (players > 1) // extra time to keep prompt jingle from cutting rudely
			{
				hole_draw(); frames(2);
				hole_draw(); frames(2);
				hole_draw(); frames(2);
				hole_draw(); frames(2);
			}
			sound_play(SFX_RESPAWN);
			balls_x[player] = tee_sx * 256;
			balls_y[player] = tee_sy - 6;
		}

	stroke_wait:
		status_player(); // indicate whose turn it is, and that input is now accepted
		do
		{
			input_poll();
			prng1(); // entropy
			hole_draw();
			frame();
			#if HOLE_SKIP
				if (gamepad & PAD_START) goto hole_skip;
			#endif
		}
		while (!(gamepad & (PAD_B | PAD_A)) && !(mouse1 & (MOUSE_L | MOUSE_R)));
	
	//stroke_swing:
		swinging = 1;
		swing_x = 0;
		swing_y = 0;
		do
		{
			input_poll();

			#define PSWING_COARSE 32
			#define PSWING_FINE    1
			#define MSWING_COARSE  8
			#define MSWING_FINE    1
			
			if (gamepad & PAD_B)
			{
				if (gamepad & PAD_LEFT ) swing_x -= PSWING_FINE;
				if (gamepad & PAD_RIGHT) swing_x += PSWING_FINE;
				if (gamepad & PAD_UP   ) swing_y -= PSWING_FINE;
				if (gamepad & PAD_DOWN ) swing_y += PSWING_FINE;
			}
			else
			{
				if (gamepad & PAD_LEFT ) swing_x -= PSWING_COARSE;
				if (gamepad & PAD_RIGHT) swing_x += PSWING_COARSE;
				if (gamepad & PAD_UP   ) swing_y -= PSWING_COARSE;
				if (gamepad & PAD_DOWN ) swing_y += PSWING_COARSE;
			}
			
			if (mouse1 & MOUSE_R)
			{
				// note: preventing overflow by testing that direction of motion has same sign
				swing_x += mouse3 * MSWING_FINE;
				swing_y += mouse2 * MSWING_FINE;
			}
			else
			{
				swing_x += mouse3 * MSWING_COARSE;
				swing_y += mouse2 * MSWING_COARSE;
			}
			
			if (swing_x >  SWING_MAX) swing_x =  SWING_MAX;
			if (swing_x < -SWING_MAX) swing_x = -SWING_MAX;
			if (swing_y >  SWING_MAX) swing_y =  SWING_MAX;
			if (swing_y < -SWING_MAX) swing_y = -SWING_MAX;
			
			prng1(); // entropy
			hole_draw();
			frame();
		}
		while ((gamepad & (PAD_B | PAD_A)) || (mouse1 & (MOUSE_L | MOUSE_R)));

		swinging = 0;
		if (
			swing_x <  SWING_MIN &&
			swing_x > -SWING_MIN &&
			swing_y <  SWING_MIN &&
			swing_y > -SWING_MIN )
			goto stroke_wait;

		stroke_add();
		if (!first_stroke) status_draw();
		status_player_hide(); // no longer taking input

		hole_draw();
		if (!first_stroke) frame_double(); // frame_double is for status_draw only
		else frame();

	//ball_fly:
		sound_play(SFX_STROKE);
		ball_x = balls_x[player];
		ball_y = balls_y[player] * 256;
		ball_vx = -swing_x / 2; // divider of swing controls max velocity
		ball_vy = -swing_y / 2;
		timeout = 0;
		rollout = 0;

		// some motion constants, higher gravity makes a "faster" game
		// DRAG should be probably be greater than WIND so that the ball will stop?
		// STICK is how fast a bounce is required to escape the floor
		#define GRAVITY 12
		#define WIND 3
		#define DRAG 6
		#define STICK 128
		// radius should be at least 2 pixels:
		//   2 pixels -> lifts ball completely off ground
		//   sqrt(4.5) pixels -> touches inner corners of all ball pixels
		//   2.5 pixels -> touches outer 4 faces
		//   sqrt(8.5) pixels -> touches outer corners of all pixels
		// chose the first, because smaller = easier to get in hole
		#define BALL_RADIUS (2*256)
		// limit amount of ejection per-frame to avoid "pop"
		// (hard hits will tend to self-correct with the bounce anyway)
		#define EJECT_MAX (BALL_RADIUS*2)
		// to keep bad physics from lasting forever, just drop the ball straight down after this many frames
		#define TIMEOUT (20*60)
		// additional timeout: drop the ball after not visually moving for this many frames
		#define ROLLOUT 16

		do
		{
			soll_sx = roll_sx;
			soll_sy = roll_sy;
			roll_sx = balls_lx[player*4];
			roll_sy = balls_y[player];

			ball_x += ball_vx;
			ball_y += ball_vy;
			ball_vy += GRAVITY;

			balls_x[player] = ball_x;
			balls_y[player] = ball_y / 256;

			if (ball_y >= (256*256))
			{
				balls_hx[player*4] = 1; // offscreen
				goto collide_skip;
			}

			// final hole water test
			if (hole == 0 && ball_y >= ((OCEAN_FLOOR-6)*256))
			{
				hole_splash(); // pause for a little splash animation
				cleared |= (1 << player);
				splash_s = 0; // clear splash
				ball_x = 256*256;
				balls_hx[player*4] = 1; // move ball offscreen
				break;
			}

			// check 5 columns of the ball for any pixel overlap with the field
			mx = (ball_x / 256) + 1; // start at left column of ball
			l = (ball_y / 256) + 5; // bottom row of ball
			px = 0; // best priority column
			k = 13; // best priority
			valley = 0;
			for (i=0; i<(5*4); i+=4)
			{
				nx = mx;
				if (mx >= 0x8000) nx = 0; // off left side, use leftmost column
				else if (mx >= 256) nx = 255; // off right side, use rightmost column
				nx = (nx + scroll_cx) & 511;
				++mx;
				j = floor_y[nx];
				if (l >= j) // possible overlap
				{
					j = l-j;
					if (j >= 2) { j = COLLIDE_PRIORITY[i];       } // centre row or above
					else        { j = COLLIDE_PRIORITY[i+(2-j)]; } // bottom two rows
					
					if (j < 13) // detect simultaneous collision on both sides
					{
						if      (i <  (2*4)) valley |= 1;
						else if (i >= (3*4)) valley |= 2;
					}
					
					// replace j with priority
					if (j < k) // store best priority column
					{
						k = j;
						px = nx;
					}
				}
			}
			if (k >= 13) goto collide_skip; // no pixel collision detected
			
			// if ball lands offscreen, don't try to collide
			// (this speeds up recovery for the next shot, but also avoids
			// a problem calculating tsx/tsy below that causes it to slip past
			// the radius of ejection and ending up in free fall)
			if (ball_x >= (256*256)) break;

			// px = index of floor
			i = floor_a[px]; // i = angle lookup for floor slope
			norm_x = read_norm_x(i);
			norm_y = read_norm_y(i);
			mx = (((px - scroll_cx) & 255) * 256) + 128; // midpoint of floor column
			nx = floor_y[px] * 256;

			// 1. eject ball from overlap with the floor plane

			// tsx/tsy = vector pointing from midpoint of floor tile to ball centre (sprite corner + 3.5 pixels)
			//tsx = (uint16)ball_x + 896 - mx; // 3.5 pixels seems to have horizontal bias on slopes
			tsx = (uint16)ball_x + 768 - mx; // 3.0 pixels instead? maybe corrects the visual bias of rounding down? unsure
			tsy = (uint16)ball_y + 896 - nx;

			// ux = distance to eject from floor plane
			//ux = BALL_RADIUS-(((norm_x * (sint32)tsx) + (norm_y * (sint32)tsy)) / 256);
			ux = BALL_RADIUS-(fmult(norm_x,tsx) + fmult(norm_y,tsy));
			if (ux > 0)
			{
				if (ux > EJECT_MAX) ux = EJECT_MAX;
				tsx = (norm_x * (sint32)ux) / 256;
				tsy = (norm_y * (sint32)ux) / 256;
				ball_x += tsx;
				ball_y += tsy;
			}

			balls_x[player] = ball_x;
			balls_y[player] = ball_y / 256;

			// 2. reflect ball velocity if against the floor plane, roll along plane if hit is too weak
			
			//tsy = ((norm_x * (sint32)ball_vx) + (norm_y * (sint32)ball_vy)) / 256; // velocity against the normal
			//tsx = ((norm_y * (sint32)ball_vx) - (norm_x * (sint32)ball_vy)) / 256; // velocity perpendicular to the normal
			tsy = fmult(norm_x,ball_vx) + fmult(norm_y,ball_vy); // velocity against the normal
			tsx = fmult(norm_y,ball_vx) - fmult(norm_x,ball_vy); // velocity perpendicular to the normal

			// stick to floor if bounce isn't strong enough
			if (tsy > -STICK && tsy < STICK )
			{
				if (tsy < 0 ) tsx -= tsx / 8; // milder attenuation of horizontal
				if      (tsx >  DRAG) tsx -= DRAG;
				else if (tsx < -DRAG) tsx += DRAG;
				else                  tsx  =    0;
				tsy = 0;
			}
			else if (tsy < 0) // bounce if we're not already heading out
			{
				if      (tsy < -400) sound_play(SFX_BOUNCE2);
				else if (tsy < -200) sound_play(SFX_BOUNCE1);
				else                 sound_play(SFX_BOUNCE0);
				tsy /= -2;
				tsx /= 2;
			}

			if (tsx == 0 && tsy == 0) break; // ball has stopped

			//ball_vx = ((norm_x * (sint32)tsy) + (norm_y * (sint32)tsx)) / 256;
			//ball_vy = ((norm_y * (sint32)tsy) - (norm_x * (sint32)tsx)) / 256;
			ball_vx = fmult(norm_x,tsy) + fmult(norm_y,tsx);
			ball_vy = fmult(norm_y,tsy) - fmult(norm_x,tsx);

			// stop ball from gaining gravity indefinitely in a valley
			if (valley >= 3)
			{
				if (ball_vy > 0) ball_vy = 0;
			}

			goto collide_done;

		collide_skip:
			// apply wind only if not colliding or on ground
			if (weather_rate_min && (prng1() < weather_wind_p))
				ball_vx += weather_wind_dir * WIND;
		
		collide_done:
			// rolling/wobble, tick it whenever the ball moves a pixel
			if (balls_lx[player*4] > roll_sx) // -x = rolling backward
			{
				ball_s -= 1; if (ball_s >= 6) ball_s = 5;
				if (balls_lx[player*4] != soll_sx || balls_y[player] != soll_sy) // secondary check for 2-frame cycle before clearing rollout
					rollout = 0;
				else
					++rollout;
			}
			else if (balls_lx[player*4] != roll_sx || balls_y[player] != roll_sy) // moved and not -x = rolling forward
			{
				ball_s += 1; if (ball_s >= 6) ball_s = 0;
				if (balls_lx[player*4] != soll_sx || balls_y[player] != soll_sy)
					rollout = 0;
				else
					++rollout;
			}
			else
			{
				++rollout; // ball is visually still?
			}

			++timeout;
			if (timeout >= TIMEOUT || rollout >= ROLLOUT)
			{
				if ((balls_hx[player*4] != 0) || (ball_y >= (256*256)))
				{
					balls_x[player] = 256*256; // force offscreen, will reload position next swing
					break; // just immediately end
				}
				// onscreen: slide down 1 pixel per frame until we're done
				t = (floor_y[(balls_wx[player*2] + scroll_cx + 3) & 511] - 6) & 255;
				while (balls_y[player] < t)
				{
					ball_y += 256;
					++balls_y[player];
					hole_draw();
					frame();
				}
				break;
			}

			hole_draw();
			frame();

		} while(1);

	//ball_landed:
		// stop the ball
		ball_s = 0;
		hole_draw();
		frame();
		hole_draw(); frames(2); // just a little extra time for last bounce sound to die down

		if ((hole != 0) && ((ball_x/256) > (hole_sx-3)) && ((ball_x/256) < (hole_sx+5)))
		{
			cleared |= (1 << player); // landed in hole!
		}
		balls_x[player] = ball_x & 0x00FFFFFF; // sanitize the 4th "padding" byte
		balls_y[player] = ball_y / 256;
		
		// next player
		for (i=0; i<4; ++i)
		{
			player = (player+1) & 3;
			if (!(cleared & (1 << player))) break;
		}
		if (i >= 4) break; // should be same as cleared >= 0x10

		// player prompt sound if mutliplayer
		if (players > 1) sound_play(SFX_PROMPTS[player]);

		if (first_stroke)
		{
			status_bar_fade_in();
			first_stroke = 0;
		}

		ball_draw_setup();
		hole_draw();
		input_poll(); // clear pending input
		frame();
	}

	#if HOLE_SKIP
		hole_skip:
	#endif

	// end of game
	while (hole == 0)
	{
		hole_draw();
		frame();
	}

	// 6. fade out status bar, remove flag
	sound_play(SFX_FLAG);
	flag_remove = 1;
	if (!first_stroke) // if they got a hole in 1, stroke status hasn't been faded in, so skip this fadeout
	{
		palette_generate(pal_sky, pal_floor, blend25(pal_text, pal_sky)); hole_draw(); frame(); hole_draw(); frame();
		palette_generate(pal_sky, pal_floor, blend50(pal_sky, pal_text)); hole_draw(); frame(); hole_draw(); frame();
		palette_generate(pal_sky, pal_floor, blend25(pal_sky, pal_text)); hole_draw(); frame(); hole_draw(); frame();
		palette_generate(pal_sky, pal_floor, pal_sky                   );
	}
	else first_stroke = 0;
	// wait for flag to finish leaving
	while (flag_remove) { hole_draw(); frame(); }

	// 7. prepare next hole
	hole_next();

	if (day) --day;

	if (day == 0)
	{
		day = DAY_TIME;
		field_set ^= 8; // day/night only
		goto field_change;
	}
	else if (transition_time == 0)
	{
		field_set = (prng() & 7) | (field_set & 8); // random but don't switch day/night
		transition_time = (prng() & 7) + 3;
	field_change:
		transition = TRANSITION_TIME;
		i = set_w[field_set];
		if (i)
		{
			weather_tile = i;
			weather_attribute_set();
		}
	}
	else
	{
		--transition_time;
	}
	weather_wind_random();
	if (hole == 0 && weather_wind_fade_dir < 0) weather_wind_fade_dir = 1; // no left wind allowed on last hole

	return; // hole_play() again
}

void fmult_test()
{
	// needed this test to verify that fmult works
	for (mx=1; mx!=0; ++mx)
	{
		tsx = (sint32)(prng() | ((uint16)prng()<<8));
		tsy = (sint32)(prng() | ((uint16)prng()<<8));
		ux = (sint32)(((sint32)tsx*tsy)/256);
		vx = fmult(tsx,tsy);
		ASSERT(ux==vx);
	}
}

//
// main`
//

void main()
{
	//fmult_test();

	// replace the common "all 0s" or "all 1s" emulator RAM initialization seed
	// with two hand-picked cases to make the title screen look nice.
	// (further entropy for subsequent holes is gathered while waiting on the
	// title screen, but I have to generate at least the title screen before user input.)
	if (seed == 0x00800000) seed = 0x00654321; // field set 7 (yellow day), 4 holes to night
	if (seed == 0x00FFFFFF) seed = 0x000D7755; // field set F (yellow night), 4 holes to day
	if (seed == 0x00FF0000) seed = 0x00654399; // field set 7 (yellow day), 5 holes to night
	if (seed == 0x00FAEFF5) seed = 0x00654321; // FDS BIOS initializes seed to this at power on?

	input_setup();

	ppu_latch(0x1000);
	ppu_fill(0x55,8*1024);

	ptr = layers_chr;
	ppu_latch(0x0000);
	ppu_load(LAYERS_CHR_SIZE);

	ptr = sprite_chr;
	ppu_latch(0x1000);
	ppu_load(SPRITE_CHR_SIZE);

	#if !SHOW_LEFT_COLUMN
		ppu_mask(0x18); // hide left column
	#endif

	title();
	while (1) hole_play();
	return; // never reached
}

// end of file
