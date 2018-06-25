#include "matrix.h"

class Camera {
public:
    gml::Vec3 pos;
    gml::Vec3 aim;
    gml::Vec3 up;
    gml::Vec3 right;
    float focalLength;
    float aspectRatio; // w:h
    float FOV; // horizontal FOV
    float aperture;
    float depthOfFocus;

    Camera()
        : pos  ( gml::Vec3( 0, 0,  0 ) )
        , aim  ( gml::Vec3( 0, 0, -1 ) )
        , up   ( gml::Vec3( 0, 1,  0 ) )
        , right( gml::Vec3( 1, 0,  0 ) )
        , focalLength( 1.0 )
        , aspectRatio( 16.0/9.0 )
        , FOV( 1.308 )
        , aperture( 1.0 )
        , depthOfFocus( 10 )
    {}

    Camera( gml::Vec3 _pos, gml::Vec3 _aim, gml::Vec3 _up, gml::Vec3 _right, float _focalLength, float _aspectRatio, float _FOV, float _aperture, float _depthOfFocus )
        : pos  ( _pos )
        , aim  ( _aim )
        , up   ( _up )
        , right( _right )
        , focalLength( _focalLength )
        , aspectRatio( _aspectRatio )
        , FOV( _FOV )
        , aperture( _aperture )
        , depthOfFocus( _depthOfFocus )
    {}

    ~Camera() {}
};
