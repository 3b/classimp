classimp
========

common lisp/cffi bindings for Open Asset Import Library (http://assimp.sourceforge.net/)

Should support assimp versions 3.0 to 3.3.x. Version to support is
determined by querying c library at compile time (or load if not
previously compiled), with errors if versions don't match at load or
runtime. (Current assimp from git will be detected as 3.3, but isn't
completely binary compatible so might have problems)


Allows (among other things) loading of the following formats:

    Collada ( .dae )
    Blender 3D ( .blend )
    3ds Max 3DS ( .3ds )
    3ds Max ASE ( .ase )
    Wavefront Object ( .obj )
    Industry Foundation Classes (IFC/Step) ( .ifc )
    XGL ( .xgl,.zgl )
    Stanford Polygon Library ( .ply )
    *AutoCAD DXF ( .dxf )
    LightWave ( .lwo )
    LightWave Scene ( .lws )
    Modo ( .lxo )
    Stereolithography ( .stl )
    DirectX X ( .x )
    AC3D ( .ac )
    Milkshape 3D ( .ms3d )
    * TrueSpace ( .cob,.scn )
