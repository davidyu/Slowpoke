#pragma once

#include <math.h>
#include <stdlib.h>

// base vector struct
template <int n> struct Vec {
    float v[n];

    float& operator[]( const int i ) {
        return this->v[i];
    }

    const float& operator[]( const int i ) const {
        return this->v[i];
    }
};

typedef Vec<2> Vec2;
typedef Vec<3> Vec3;
typedef Vec<4> Vec4;

// specialized definitions for Vec2, Vec3, and Vec4
template<> struct Vec<2> {
    union {
        float vals[2];
        struct { float x, y; };
        struct { float r, g; };
        struct { float u, v; };
    };

    Vec( float _x, float _y )
        : x( _x ) , y( _y ) {}

    Vec()
        : x( 0 ), y ( 0 ) {}

    float& operator[]( const int i ) {
        return this->vals[i];
    }

    const float& operator[]( const int i ) const {
        return this->vals[i];
    }
};

template<> struct Vec<3> {
    union {
        float v[3];
        struct { float x, y, z; };
        struct { float r, g, b; };
        Vec<2> xy;
    };

    Vec( float _x, float _y, float _z )
        : x( _x ), y( _y ), z( _z ) {}

    Vec()
        : x( 0 ), y( 0 ), z( 0 ) {}

    float& operator[]( const int i ) {
        return this->v[i];
    }

    const float& operator[]( const int i ) const {
        return this->v[i];
    }
};

template<> struct Vec<4> {
    union {
        float v[4];
        struct { float x, y, z, w; };
        struct { float r, g, b, a; };
        Vec<2> xy;
        Vec<3> xyz;
        Vec<3> rgb;
    };

    Vec( float _x, float _y, float _z, float _w )
        : x( _x ), y( _y ), z( _z ), w( _w ) {}

    Vec()
        : x( 0 ), y( 0 ), z( 0 ), w( 1 ) {}

    float& operator[]( const int i ) {
        return this->v[i];
    }

    const float& operator[]( const int i ) const {
        return this->v[i];
    }
};

// generic operators
template <int n> Vec<n>  operator+          ( const Vec<n> lhs , const Vec<n> rhs );
template <int n> Vec<n>& operator+=         ( Vec<n>& lhs      , const Vec<n> rhs );
template <int n> Vec<n>  operator-          ( const Vec<n> lhs , const Vec<n> rhs );
template <int n> Vec<n>& operator-=         ( Vec<n>& lhs      , const Vec<n> rhs );
template <int n> Vec<n>  operator-          ( const Vec<n> lhs                    );
template <int n> Vec<n>  operator*          ( const Vec<n> lhs , const Vec<n> rhs );
template <int n> Vec<n>& operator*=         ( Vec<n>& lhs      , const Vec<n> rhs );
template <int n> Vec<n>  operator*          ( const Vec<n> lhs , const float rhs  );
template <int n> Vec<n>& operator*=         ( Vec<n>& lhs      , const float rhs  );
template <int n> Vec<n>  operator*          ( const float lhs  , const Vec<n> rhs );
template <int n> Vec<n>  operator/          ( const Vec<n> lhs , const Vec<n> rhs );
template <int n> Vec<n>& operator/=         ( Vec<n>& lhs      , const Vec<n> rhs );
template <int n> Vec<n>  operator/          ( const Vec<n> lhs , const float rhs  );
template <int n> Vec<n>& operator/=         ( Vec<n>& lhs      , const float rhs  );
template <int n> Vec<n>  operator^          ( const Vec<n> lhs , const Vec<n> rhs );
template <int n> Vec<n>& operator^=         ( Vec<n>& lhs      , const Vec<n> rhs );
template <int n> Vec<n>  operator^          ( const Vec<n> lhs , const float rhs  );
template <int n> Vec<n>& operator^=         ( Vec<n>& lhs      , const float rhs  );

// vector-specific operations
template <int n> float   dot                ( const Vec<n> lhs, const Vec<n> rhs );
template <int n> Vec<n>  cross              ( const Vec<n> lhs, const Vec<n> rhs );
                 float   cross2D            ( const Vec2   lhs, const Vec2   rhs ); // only for vec2s
template <int n> float   lengthsq           ( const Vec<n> in );
template <int n> float   length             ( const Vec<n> in );
template <int n> Vec<n>  normalize          ( const Vec<n> in );

// free-floating constructors
template <int n> Vec<n>  zero               ( void );
template <int n> void    zero               ( Vec<n>& in_out );

// picks points on the surface of the n-sphere (n minus one-sphere)
template <int n> Vec<n>  randomOnSphere ( float radius = 1.f );
template <int n> void    randomOnSphere ( Vec<n>& in_out, float radius = 1.f );

// picks points inside the n-sphere (n minus one-sphere)
template <int n> Vec<n>  randomInSphere ( float radius = 1.f );
template <int n> void    randomInSphere ( Vec<n>& in_out, float radius = 1.f );

// picks a point inside a 2D disk
                 Vec2    randomInDisk   ( float radius = 1.f );

// generic operator definitions
template <int n>
Vec<n> operator+ ( const Vec<n> lhs, const Vec<n> rhs ) {
    Vec<n> sum;
    for ( int i = 0; i < n; i++ ) {
        sum[i] = lhs[i] + rhs[i];
    }
    return sum;
}

template <int n>
Vec<n>& operator+= ( Vec<n>& lhs, const Vec<n> rhs ) {
    for ( int i = 0; i < n; i++ ) {
        lhs[i] += rhs[i];
    }
    return lhs;
}

template <int n>
Vec<n> operator- ( const Vec<n> lhs, const Vec<n> rhs ) {
    Vec<n> difference;
    for ( int i = 0; i < n; i++ ) {
        difference[i] = lhs[i] - rhs[i];
    }
    return difference;
}

template <int n>
Vec<n>& operator-= ( Vec<n>& lhs, const Vec<n> rhs ) {
    for ( int i = 0; i < n; i++ ) {
        lhs[i] -= rhs[i];
    }
    return lhs;
}

template <int n>
Vec<n> operator- ( const Vec<n> lhs ) {
    Vec<n> negation;
    for ( int i = 0; i < n; i++ ) {
        negation[i] = -lhs[i];
    }
    return negation;
}

template <int n>
Vec<n> operator* ( const Vec<n> lhs, const Vec<n> rhs ) {
    Vec<n> product;
    for ( int i = 0; i < n; i++ ) {
        product[i] = lhs[i] * rhs[i];
    }
    return product;
}

template <int n>
Vec<n>& operator*= ( Vec<n>& lhs, const Vec<n> rhs ) {
    for ( int i = 0; i < n; i++ ) {
        lhs[i] *= rhs[i];
    }
    return lhs;
}

template <int n>
Vec<n> operator* ( const Vec<n> lhs, const float rhs ) {
    Vec<n> scaled;
    for ( int i = 0; i < n; i++ ) {
        scaled[i] = lhs[i] * rhs;
    }
    return scaled;
}

template <int n>
Vec<n>& operator*= ( Vec<n>& lhs, const float rhs ) {
    for ( int i = 0; i < n; i++ ) {
        lhs[i] *= rhs;
    }
    return lhs;
}

template <int n>
Vec<n> operator* ( const float lhs, const Vec<n> rhs ) {
    return operator*( rhs, lhs );
}

template <int n>
Vec<n> operator/ ( const Vec<n> lhs, const Vec<n> rhs ) {
    Vec<n> quotient;
    for ( int i = 0; i < n; i++ ) {
        quotient[i] = lhs[i] / rhs[i];
    }
    return quotient;
}

template <int n>
Vec<n>& operator/= ( Vec<n>& lhs, const Vec<n> rhs ) {
    for ( int i = 0; i < n; i++ ) {
        lhs[i] /= rhs[i];
    }
    return lhs;
}

template <int n>
Vec<n> operator/ ( const Vec<n> lhs, const float rhs ) {
    Vec<n> scaled;
    for ( int i = 0; i < n; i++ ) {
        scaled[i] = lhs[i] / rhs;
    }
    return scaled;
}

template <int n>
Vec<n>& operator/= ( Vec<n>& lhs, const float rhs ) {
    for ( int i = 0; i < n; i++ ) {
        lhs[i] /= rhs;
    }
    return lhs;
}

template <int n>
Vec<n>& operator^= ( Vec<n>& lhs, const Vec<n> rhs ) {
    for ( int i = 0; i < n; i++ ) {
        lhs[i] = pow( lhs[i], rhs[i] );
    }
    return lhs;
}

template <int n>
Vec<n> operator^ ( const Vec<n> lhs, const Vec<n> rhs ) {
    Vec<n> raised;
    for ( int i = 0; i < n; i++ ) {
        raised[i] = pow( lhs[i], rhs[i] );
    }
    return raised;
}

template <int n>
Vec<n>& operator^= ( Vec<n>& lhs, const float rhs ) {
    for ( int i = 0; i < n; i++ ) {
        lhs[i] = pow( lhs[i], rhs );
    }
    return lhs;
}

template <int n>
Vec<n> operator^ ( const Vec<n> lhs, const float rhs ) {
    Vec<n> raised;
    for ( int i = 0; i < n; i++ ) {
        raised[i] = pow( lhs[i], rhs );
    }
    return raised;
}

template <int n>
float dot( const Vec<n> lhs, const Vec<n> rhs ) {
    float dp = 0.f;
    for ( int i = 0; i < n; i++ ) {
        dp += lhs[i] * rhs[i];
    }
    return dp;
}

template <int n>
float lengthsq( const Vec<n> in ) {
    float sum = 0.f;
    for ( int i = 0; i < n; i++ ) {
        sum += in[i] * in[i];
    }
    return sum;
}

template <int n>
float length( const Vec<n> in ) {
    return sqrt( lengthsq( in ) );
}

template <int n>
Vec<n> normalize( const Vec<n> in ) {
    return in / length( in );
}

template <int n>
Vec<n> cross( const Vec<n> lhs, const Vec<n> rhs ) {
    static_assert( n == 3, "cross only defined for Vec<3>!" );
    return { lhs[1] * rhs[2] - lhs[2] * rhs[1]
           , lhs[2] * rhs[0] - lhs[0] * rhs[2]
           , lhs[0] * rhs[1] - lhs[1] * rhs[0] };
}

inline float cross2D( const Vec2 lhs, const Vec2 rhs ) {
    return lhs[0] * rhs[1] - lhs[1] * rhs[0];
}

template <int n>
Vec<n> zero() {
    Vec<n> out;
    for ( int i = 0; i < n; i++ ) {
        out[i] = 0;
    }
    return out;
}

template <int n>
void zero( Vec<n>& in_out ) {
    for ( int i = 0; i < n; i++ ) {
        in_out[i] = 0;
    }
}

template <int n>
Vec<n> randomOnSphere( float radius ) {
    Vec<n> out;
    for ( int i = 0; i < n; i++ ) {
        out[i] = 2.0 * drand48() - 1;
    }
    out *= ( radius / length( out ) );
    return out;
}

template <int n>
void randomOnSphere( Vec<n>& in_out, float radius ) {
    for ( int i = 0; i < n; i++ ) {
        in_out[i] = 2.0 * drand48() - 1;
    }
    in_out *= ( radius / length( in_out ) );
}

template <int n>
Vec<n> randomInSphere( float radius ) {
    float rsq = radius * radius;
    Vec<n> out;
    do {
        for ( int i = 0; i < n; i++ ) {
            out[i] = radius * ( 2.0 * drand48() - 1 );
        }
    } while ( lengthsq( out ) > rsq );
    return out;
}

template <>
inline Vec<3> randomInSphere( float radius ) {
    float u1 = 2.0 * drand48() - 1;
    float u2 = drand48();
    float r = sqrt( 1.0f - u1 * u1 );
    float theta = 2.0f * M_PI * u2;

    return radius * Vec<3>( r * cosf( theta ), r * sinf( theta ), u1 );
}

template <int n>
void randomInSphere( Vec<n>& in_out, float radius ) {
    float rsq = radius * radius;
    do {
        for ( int i = 0; i < n; i++ ) {
            in_out[i] = radius * ( 2.0 * drand48() - 1 );
        }
    } while ( lengthsq( in_out ) > rsq );
}

template <>
inline void randomInSphere( Vec<3>& in_out, float radius ) {
    float u1 = 2.0 * drand48() - 1;
    float u2 = drand48();
    float r = sqrt( 1.0f - u1 * u1 );
    float theta = 2.0f * M_PI * u2;

    in_out = { r * cosf( theta ), r * sinf( theta ), u1 };
}

inline Vec2 randomInDisk( float radius ) {
    float t = 2 * M_PI * drand48();
    float u = drand48() * drand48();
    float r = u > 1 ? 2 - u : u;
    return Vec2( radius * r * cos( t ), radius * r * sin( t ) );
}
