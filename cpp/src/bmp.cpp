#include "bmp.h"
#include <cstring>
#include <cmath>
#include <fstream>

BMP::BMP( int w, int h ) {
    InfoHeader.Width = w;
    InfoHeader.Height = h;
    InfoHeader.ImageSize = w * h * 3;
    Image = new char[ InfoHeader.ImageSize ];

    InfoHeader.ImageSize = FileHeader.Offset + InfoHeader.ImageSize;

    // initialize image to black
    memset( Image, 0, InfoHeader.ImageSize );
}

BMP::~BMP() {
    delete[] Image;
}

void BMP::Write( std::string path ) {
    std::ofstream out( path, std::ofstream::binary );

    out.write( (char *) &FileHeader, sizeof( BMPFILEHEADER ) );
    out.write( (char *) &InfoHeader, sizeof( BMPINFOHEADER ) );
    out.write( Image, InfoHeader.ImageSize );

    out.close();
}

void BMP::SetPixel( int x, int y, Color color ) {
    int pos = 3 * ( x + y * InfoHeader.Width );

    // recall that bitmap data is actually flipped when stored on disk
    Image[ pos ]     = round( fmin( 1.f, color.b ) * 255 );
    Image[ pos + 1 ] = round( fmin( 1.f, color.g ) * 255 );
    Image[ pos + 2 ] = round( fmin( 1.f, color.r ) * 255 );
}
