#pragma once

#include "stdint.h"
#include <string>
#include "color.hpp"

struct BMPINFOHEADER {
    uint32_t Size = sizeof( BMPINFOHEADER );
    uint32_t Width;
    uint32_t Height;
    uint16_t Planes = 1;
    uint16_t BitCount = 24;       // 16.7 million colors
    uint32_t Compression = 0;
    uint32_t ImageSize;
    uint32_t XPelsPerMeter = 0;
    uint32_t YPelsPerMeter = 0;
    uint32_t UsedColors = 0;      // set to zero to indicate we use BitCount to determine no. of colors used
    uint32_t ImportantColors = 0; // set to zero to indicate that all colors are important
} __attribute__ (( packed ));

struct BMPFILEHEADER {
    uint16_t Type = 19778;        // 'BM' characters as identifier ('B'+('M'<<8)) == 19778
    uint32_t Size;                // size of the entire .bmp file, must be filled out at runtime
    uint32_t Reserved = 0;
    uint32_t Offset = sizeof( BMPFILEHEADER ) + sizeof( BMPINFOHEADER ); // offset of image data block
} __attribute__ (( packed ));

static_assert( sizeof( BMPINFOHEADER ) == 40, "size of BMPINFOHEADER is wrong!" );

class BMP {
private:
    BMPFILEHEADER FileHeader;
    BMPINFOHEADER InfoHeader;
    char * Image;

public:
    BMP( int w, int h );
    ~BMP();

    void Write( std::string path );
    void SetPixel( int x, int y, Color color );
};
