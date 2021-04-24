! Description:
!
! Input Parameters:
! None.
!
! Output Parameters:
! Many public constants are defined in "netcdf_constants.f90". The names follow
! the Fortran 77 names, with nf90_ used as a prefix instead of nf_77.
! Functions are made accessable through PUBLIC:: statements in "netcdf_visibility.f90".
! Only the functions listed in this file are available in the interface.
!
! References and Credits:
! Written by
! Robert Pincus
! Cooperative Institue for Meteorological Satellite Studies
! University of Wisconsin - Madison
! 1225 W. Dayton St.
! Madison, Wisconsin 53706
! Robert.Pincus@ssec.wisc.edu
!
! Design Notes:
! Module elements are private by default.
! Many functions have optional arguments. In order to keep the interface easy to use,
! we've reordered the arguments (relative to the F77 interface) in some functions. 
! The external datatype of attributes is the same as the internal type.
! By default, data is read from or put into the lowest part of the netCDF array with stride 1.
! We've made heavy use of overloading, especially in the variable put and get routines. 
! A single function exists for putting all variables; a single function exists for getting
! variables.
! Text variables must be treated somewhat differently. When a character variable is defined, the
! fastest-varying index (first index in Fortran) must be the maxiumu length of the character
! string. N dimensional arrays of strings passed to the put or get functions are written/read
! from dimensions 2:N+1. The number of values along the first dimension is determined by the
! length of the argument character string.
!
! NOTE: The netcdf4.f90 version of this file was created by Ed
! Hartnett in 2006 to support the netCDF-4 API.
!
 module netcdf
  use typesizes, only: OneByteInt, TwoByteInt, FourByteInt, EightByteInt, &
                       FourByteReal, EightByteReal
  implicit none
  private
  ! as of version 4.4, the following are merged:
  ! - netcdf_constants.f90 and netcdf4_constants.f90
  !
  ! Update the dependencies in the Makefile.am when modifying the list of
  ! #included files.
  !
  ! external netcdf data types:
  !
  integer, parameter, public :: &
    nf90_byte = 1, &
    nf90_int1 = nf90_byte, &
    nf90_char = 2, &
    nf90_short = 3, &
    nf90_int2 = nf90_short, &
    nf90_int = 4, &
    nf90_int4 = nf90_int, &
    nf90_float = 5, &
    nf90_real = nf90_float, &
    nf90_real4 = nf90_float, &
    nf90_double = 6, &
    nf90_real8 = nf90_double
  !
  ! default fill values:
  !
  character (len = 1), parameter, public :: &
    nf90_fill_char = achar(0)
  integer (kind = selected_int_kind(2)), parameter, public :: &
    nf90_fill_byte = -127, &
    nf90_fill_int1 = nf90_fill_byte
  integer (kind = selected_int_kind(4)), parameter, public :: &
    nf90_fill_short = -32767, &
    nf90_fill_int2 = nf90_fill_short
  integer (kind = selected_int_kind(9)), parameter, public :: &
    nf90_fill_int = -2147483647
  real (kind = selected_real_kind(P =  6, R =  37)), parameter, public :: &
    nf90_fill_float = 9.9692099683868690e+36, &
    nf90_fill_real = nf90_fill_float, &
    nf90_fill_real4 = nf90_fill_float
  real (kind = selected_real_kind(P =  13, R =  307)), parameter, public :: &
    nf90_fill_double = 9.9692099683868690e+36, &
    nf90_fill_real8 = nf90_fill_double
  !
  ! mode flags for opening and creating a netcdf dataset:
  !
  integer, parameter, public :: &
    nf90_nowrite = 0, &
    nf90_write = 1, &
    nf90_clobber = 0, &
    nf90_noclobber = 4, &
    nf90_fill = 0, &
    nf90_nofill = 256, &
    nf90_64bit_offset = 512, &
    nf90_64bit_data = 32, &
    nf90_cdf5 = nf90_64bit_data, &
    nf90_lock = 1024, &
    nf90_share = 2048, &
    nf90_diskless = 8, &
    nf90_mmap = 16
  integer, parameter, public :: &
    nf90_sizehint_default = 0, &
    nf90_align_chunk = -1
  !
  ! size argument for defining an unlimited dimension:
  !
  integer, parameter, public :: nf90_unlimited = 0
  !
  ! global attribute id:
  !
  integer, parameter, public :: nf90_global = 0
  !
  ! implementation limits:
  !
  integer, parameter, public :: &
    nf90_max_dims = 1024, &
    nf90_max_attrs = 8192, &
    nf90_max_vars = 8192, &
    nf90_max_name = 256, &
    nf90_max_var_dims = 1024
  !
  ! error codes:
  !
  integer, parameter, public :: &
    nf90_noerr = 0, & ! No Error
    nf90_ebadid = -33, & ! Not a valid ID
    nf90_eexist = -35, & ! netcdf file exists && NF90_NOCLOBBER
    nf90_einval = -36, & ! Invalid Argument
    nf90_eperm = -37, & ! Write to read only
    nf90_enotindefine = -38, & ! Operation not allowed in data mode
    nf90_eindefine = -39, & ! Operation not allowed in define mode
    nf90_einvalcoords = -40, & ! Index exceeds dimension bound
    nf90_emaxdims = -41, & ! nf90_max_dims exceeded
    nf90_enameinuse = -42, & ! String match to name in use
    nf90_enotatt = -43, & ! Attribute not found
    nf90_emaxatts = -44, & ! nf90_max_attrs exceeded
    nf90_ebadtype = -45, & ! Not a netcdf data type
    nf90_ebaddim = -46, & ! Invalid dimension id or name
    nf90_eunlimpos = -47, & ! nf90_unlimited in the wrong index
    nf90_emaxvars = -48, & ! nf90_max_vars exceeded
    nf90_enotvar = -49, & ! The variable ID is invalid for the specified netCDF dataset.
    nf90_eglobal = -50, & ! Action prohibited on nf90_global varid
    nf90_enotnc = -51, & ! Not a netcdf file
    nf90_ests = -52, & ! In Fortran, string too short
    nf90_emaxname = -53, & ! nf90_max_name exceeded
    nf90_eunlimit = -54, & ! nf90_unlimited size already in use
    nf90_enorecvars = -55, & ! nc_rec op when there are no record vars
    nf90_echar = -56, & ! Attempt to convert between text & numbers
    nf90_eedge = -57, & ! Start+count exceeds dimension bound
    nf90_estride = -58, & ! Illegal stride
    nf90_ebadname = -59, & ! Attribute or variable name contains illegal characters
    nf90_erange = -60, & ! Math result not representable
    nf90_enomem = -61, & ! Memory allocation (malloc) failure
    nf90_evarsize = -62, & ! One or more variable sizes violate format constraints
    nf90_edimsize = -63, & ! Invalid dimension size
    nf90_etrunc = -64, & ! File likely truncated or possibly corrupted
    nf90_eaxistype = -65 ! Unknown axis type.
  !
  ! more error codes for DAP
  !
  integer, parameter, public :: &
    nf90_edap = -66, & ! Generic DAP error
    nf90_ecurl = -67, & ! Generic libcurl error
    nf90_eio = -68, & ! Generic IO error
    nf90_enodata = -69, & ! Attempt to access variable with no data
    nf90_edapsvc = -70, & ! DAP server error
    nf90_edas = -71, & ! Malformed or inaccessible DAS
    nf90_edds = -72, & ! Malformed or inaccessible DDS
    nf90_edatadds = -73, & ! Malformed or inaccessible DATADDS
    nf90_edapurl = -74, & ! Malformed DAP URL
    nf90_edapconstraint = -75, & ! Malformed DAP Constrain
    nf90_etranslation = -76, & ! Untranslatable construct
    nf904_first_error = -100
  !
  ! error codes for netCDF-4
  !
integer, parameter, public :: &
    nf90_ehdferr = -101, & ! Error at HDF5 layer.
    nf90_ecantread = -102, & ! Can't read. 
    nf90_ecantwrite = -103, & ! Can't write. 
    nf90_ecantcreate = -104, & ! Can't create. 
    nf90_efilemeta = -105, & ! Problem with file metadata.
    nf90_edimmeta = -106, & ! Problem with dimension metadata.
    nf90_eattmeta = -107, & ! Problem with attribute metadata.
    nf90_evarmeta = -108, & ! Problem with variable metadata.
    nf90_enocompound = -109, & ! Not a compound type.
    nf90_eattexists = -110, & ! Attribute already exists.
    nf90_enotnc4 = -111, & ! Attempting netcdf-4 operation on netcdf-3 file.
    nf90_estrictnc3 = -112, & ! Attempting netcdf-4 operation on strict nc3 netcdf-4 file.
    nf90_enotnc3 = -113, & ! Attempting netcdf-3 operation on netcdf-4 file.
    nf90_enopar = -114, & ! Parallel operation on file opened for non-parallel access.
    nf90_eparinit = -115, & ! Error initializing for parallel access.
    nf90_ebadgrpid = -116, & ! Bad group ID.
    nf90_ebadtypid = -117, & ! Bad type ID.
    nf90_etypdefined = -118, & ! Type has already been defined and may not be edited.
    nf90_ebadfield = -119, & ! Bad field ID.
    nf90_ebadclass = -120, & ! Bad class.
    nf90_emaptype = -121, & ! Mapped access for atomic types only.
    nf90_elatefill = -122, & ! Attempt to define fill value when data already exists.
    nf90_elatedef = -123, & ! Attempt to define var properties, like deflate, after enddef.
    nf90_edimscale = -124, & ! Probem with HDF5 dimscales.
    nf90_enogrp = -125, & ! No group found.
    nf90_estorage = -126, & ! Can't specify both contiguous and chunking.
    nf90_ebadchunk = -127, & ! Bad chunksize.
    nf90_enotbuilt = -128, & ! Attempt to use feature that was not turned on when netCDF was built.
    nf90_ediskless = -129, & ! Error in using diskless access.
    nf90_ecantextend = -130, & ! Attempt to extend dataset during ind. I/O operation.
    nf90_empi = -131, & ! MPI operation failed.
    nf904_last_error = -131
  !
  ! error handling modes:
  !
  integer, parameter, public :: &
    nf90_fatal = 1, &
    nf90_verbose = 2
  !
  ! format version numbers:
  !
  integer, parameter, public :: &
    nf90_format_classic = 1, &
    nf90_format_64bit = 2, &
    nf90_format_64bit_offset = nf90_format_64bit, &
    nf90_format_64bit_data = 5, &
    nf90_format_cdf5 = nf90_format_64bit_data, &
    nf90_format_netcdf4 = 3, &
    nf90_format_netcdf4_classic = 4
! extra data types:
integer, parameter, public :: &
     nf90_ubyte = 7, &
     nf90_ushort = 8, &
     nf90_uint = 9, &
     nf90_int64 = 10, &
     nf90_uint64 = 11, &
     nf90_string = 12, &
     nf90_vlen = 13, &
     nf90_opaque = 14, &
     nf90_enum = 15, &
     nf90_compound = 16
! extra default fill values:
integer (kind = selected_int_kind(4)), parameter, public :: &
     nf90_fill_ubyte = 255, &
     nf90_fill_uint1 = nf90_fill_ubyte
integer (kind = selected_int_kind(9)), parameter, public :: &
     nf90_fill_ushort = 65535, &
     nf90_fill_uint2 = nf90_fill_ushort
integer (kind = selected_int_kind(18)), parameter, public :: &
     nf90_fill_uint = 4294967295_EightByteInt
! Extra file create mode flags.
integer, parameter, public :: &
     nf90_netcdf4 = 4096, &
     nf90_hdf5 = 4096, & ! deprecated
     nf90_classic_model = 256
! Flags for parallel access.
integer, parameter, public :: nf90_independent = 0, nf90_collective = 1
! Flags for parallel I/O.
integer, parameter, public :: nf90_mpiio = 8192, nf90_mpiposix = 16384, &
     nf90_pnetcdf = 32768
! Extra variable flags.
integer, parameter, public :: &
     nf90_chunk_seq = 0, &
     nf90_chunk_sub = 1, &
     nf90_chunk_sizes = 2, &
     nf90_endian_native = 0, &
     nf90_endian_little = 1, &
     nf90_endian_big = 2, &
     nf90_chunked = 0, &
     nf90_notcontiguous = 0, &
     nf90_contiguous = 1, &
     nf90_compact = 2, &
     nf90_nochecksum = 0, &
     nf90_fletcher32 = 1, &
     nf90_noshuffle = 0, &
     nf90_shuffle = 1, &
     nf90_szip_ec_option_mask = 4, &
     nf90_szip_nn_option_mask = 32
! This is the position of NC_NETCDF4 in cmode, counting from the
! right, starting (uncharacteristically for fortran) at 0. It's needed
! for the BTEST function calls.
integer, parameter, private :: NETCDF4_BIT = 12
  character (len = 80), external :: nf_inq_libvers, nf_strerror
  ! Control routines
  integer, external :: nf_open, nf__open, nf_create, nf__create, &
                                    nf_enddef, nf__enddef, nf_set_fill, nf_redef, &
                                    nf_sync, nf_abort, nf_close, &
                                    ! These are used only in undocumented functions
                                    nf_set_base_pe, nf_inq_base_pe, &
                                    nf__create_mp, nf__open_mp, nf_delete, &
                                    nf_inq_format
  ! File level inquiry
  integer, external :: nf_inq
  ! File path inquiry
  integer, external :: nf_inq_path
  ! Dimension routines nf_inq_dim
  integer, external :: nf_def_dim, nf_inq_dimid, nf_rename_dim, nf_inq_dim
  ! Attribute routines
  integer, external :: nf_copy_att, nf_rename_att, nf_del_att, &
                                    nf_inq_att, nf_inq_attid, nf_inq_attname
  integer, external :: nf_put_att_text, nf_get_att_text, &
                                    nf_put_att_int1, nf_put_att_int2, nf_put_att_int, &
                                    nf_put_att_int64, &
                                    nf_get_att_int1, nf_get_att_int2, nf_get_att_int, &
                                    nf_get_att_int64, &
                                    nf_put_att_real, nf_get_att_real, &
                                    nf_put_att_double, nf_get_att_double
  ! Variable routines
  integer, external :: nf_def_var, nf_inq_varid, nf_inq_var, nf_rename_var
  integer, external :: nf_put_var1_text, nf_get_var1_text, &
                                    nf_put_var1_int1, nf_put_var1_int2, nf_put_var1_int, &
                                    nf_get_var1_int1, nf_get_var1_int2, nf_get_var1_int, &
                                    nf_put_var1_real, nf_get_var1_real, &
                                    nf_put_var1_double, nf_get_var1_double
  integer, external :: nf_put_vars_text, nf_get_vars_text, &
                                    nf_put_vars_int1, nf_put_vars_int2, nf_put_vars_int, &
                                    nf_get_vars_int1, nf_get_vars_int2, nf_get_vars_int, &
                                    nf_put_vars_real, nf_get_vars_real, &
                                    nf_put_vars_double, nf_get_vars_double
  integer, external :: nf_put_vara_text, nf_get_vara_text, &
                                    nf_put_vara_int1, nf_put_vara_int2, nf_put_vara_int, &
                                    nf_get_vara_int1, nf_get_vara_int2, nf_get_vara_int, &
                                    nf_put_vara_real, nf_get_vara_real, &
                                    nf_put_vara_double, nf_get_vara_double
  integer, external :: nf_put_varm_text, nf_get_varm_text, &
                                    nf_put_varm_int1, nf_put_varm_int2, nf_put_varm_int, &
                                    nf_get_varm_int1, nf_get_varm_int2, nf_get_varm_int, &
                                    nf_put_varm_real, nf_get_varm_real, &
                                    nf_put_varm_double, nf_get_varm_double
! This is part of the netCDF-4 fortran 90 API.
! Copyright 2006, UCAR
! Ed Hartnett
  ! Extra netCDF-4 functions
  integer, external :: nf_create_par, nf_open_par, nf_var_par_access, &
       nf_inq_ncid, nf_inq_grps, nf_inq_grpname, nf_inq_grpname_full, &
       nf_inq_grpname_len, nf_inq_grp_parent, nf_inq_grp_ncid, nf_inq_grp_full_ncid, nf_inq_varids, &
       nf_inq_dimids, nf_inq_typeids, nf_inq_typeid, nf_def_grp, nf_def_compound, &
       nf_insert_compound, nf_insert_array_compound, nf_inq_type, &
       nf_inq_compound, nf_inq_compound_name, nf_inq_compound_size, &
       nf_inq_compound_nfields, nf_inq_compound_field, &
       nf_inq_compound_fieldname, nf_inq_compound_fieldindex, &
       nf_inq_compound_fieldtype, nf_inq_compound_fieldndims, &
       nf_inq_compound_fielddim_sizes, nf_inq_compound_fieldoffset, &
       nf_def_vlen, nf_inq_vlen, nf_free_vlen, nf_inq_user_type, &
       nf_def_enum, nf_insert_enum, nf_inq_enum, nf_inq_enum_member, &
       nf_inq_enum_ident, nf_def_opaque, nf_inq_opaque, &
       nf_def_var_chunking, nf_def_var_deflate, &
       nf_def_var_fletcher32, nf_inq_var_chunking, nf_inq_var_deflate, &
       nf_inq_var_fletcher32, nf_inq_var_endian, nf_def_var_endian, &
       nf_def_var_fill, nf_inq_var_fill, nf_get_att, nf_put_att, &
       nf_put_vars, nf_get_vars, nf_put_vlen_element, &
       nf_put_var1_int64, nf_put_vara_int64, nf_put_vars_int64, &
       nf_put_varm_int64, nf_put_var_int64, nf_get_var1_int64, &
       nf_get_vara_int64, nf_get_vars_int64, nf_get_varm_int64, &
       nf_get_var_int64, nf_get_chunk_cache, nf_set_chunk_cache, &
       nf_inq_var_szip, nf_def_var_szip, nf_free_vlens, nf_free_string, &
       nf_set_var_chunk_cache, nf_get_var_chunk_cache, nf_rename_grp, &
       nf_def_var_filter, nf_inq_var_filter
  ! Overloaded variable functions
  interface nf90_def_var
    module procedure nf90_def_var_Scalar, nf90_def_var_oneDim, nf90_def_var_ManyDims
  end interface ! nf90_def_var
  ! Overloaded attribute functions
  interface nf90_put_att
    module procedure nf90_put_att_text, &
                     nf90_put_att_OneByteInt, nf90_put_att_TwoByteInt, &
                     nf90_put_att_FourByteInt, nf90_put_att_EightByteInt, &
                     nf90_put_att_FourByteReal, nf90_put_att_EightByteReal
    module procedure nf90_put_att_one_OneByteInt, nf90_put_att_one_TwoByteInt, &
                     nf90_put_att_one_FourByteInt, nf90_put_att_one_EightByteInt, &
                     nf90_put_att_one_FourByteReal, nf90_put_att_one_EightByteReal
  end interface !nf90_put_att
  interface nf90_get_att
    module procedure nf90_get_att_text, &
                     nf90_get_att_OneByteInt, nf90_get_att_TwoByteInt, &
                     nf90_get_att_FourByteInt, nf90_get_att_EightByteInt, &
                     nf90_get_att_FourByteReal, nf90_get_att_EightByteReal
    module procedure nf90_get_att_one_OneByteInt, nf90_get_att_one_TwoByteInt, &
                     nf90_get_att_one_FourByteInt, nf90_get_att_one_EightByteInt, &
                     nf90_get_att_one_FourByteReal, nf90_get_att_one_EightByteReal
  end interface ! nf90_get_att
  ! Overloaded variable functions
  interface nf90_put_var
    module procedure nf90_put_var_text, &
                     nf90_put_var_OneByteInt, nf90_put_var_TwoByteInt, &
                     nf90_put_var_FourByteInt, nf90_put_var_EightByteInt, &
                     nf90_put_var_FourByteReal, nf90_put_var_EightByteReal
    module procedure nf90_put_var_1D_text, &
                     nf90_put_var_1D_OneByteInt, nf90_put_var_1D_TwoByteInt, &
                     nf90_put_var_1D_FourByteInt, nf90_put_var_1D_EightByteInt, &
                     nf90_put_var_1D_FourByteReal, nf90_put_var_1D_EightByteReal
    module procedure nf90_put_var_2D_text, &
                     nf90_put_var_2D_OneByteInt, nf90_put_var_2D_TwoByteInt, &
                     nf90_put_var_2D_FourByteInt, nf90_put_var_2D_EightByteInt, &
                     nf90_put_var_2D_FourByteReal, nf90_put_var_2D_EightByteReal
    module procedure nf90_put_var_3D_text, &
                     nf90_put_var_3D_OneByteInt, nf90_put_var_3D_TwoByteInt, &
                     nf90_put_var_3D_FourByteInt, nf90_put_var_3D_EightByteInt, &
                     nf90_put_var_3D_FourByteReal, nf90_put_var_3D_EightByteReal
    module procedure nf90_put_var_4D_text, &
                     nf90_put_var_4D_OneByteInt, nf90_put_var_4D_TwoByteInt, &
                     nf90_put_var_4D_FourByteInt, nf90_put_var_4D_EightByteInt, &
                     nf90_put_var_4D_FourByteReal, nf90_put_var_4D_EightByteReal
    module procedure nf90_put_var_5D_text, &
                     nf90_put_var_5D_OneByteInt, nf90_put_var_5D_TwoByteInt, &
                     nf90_put_var_5D_FourByteInt, nf90_put_var_5D_EightByteInt, &
                     nf90_put_var_5D_FourByteReal, nf90_put_var_5D_EightByteReal
    module procedure nf90_put_var_6D_text, &
                     nf90_put_var_6D_OneByteInt, nf90_put_var_6D_TwoByteInt, &
                     nf90_put_var_6D_FourByteInt, nf90_put_var_6D_EightByteInt, &
                     nf90_put_var_6D_FourByteReal, nf90_put_var_6D_EightByteReal
    module procedure nf90_put_var_7D_text, &
                     nf90_put_var_7D_OneByteInt, nf90_put_var_7D_TwoByteInt, &
                     nf90_put_var_7D_FourByteInt, nf90_put_var_7D_EightByteInt, &
                     nf90_put_var_7D_FourByteReal, nf90_put_var_7D_EightByteReal
  end interface ! nf90_put_var
  interface nf90_get_var
    module procedure nf90_get_var_text, &
                     nf90_get_var_OneByteInt, nf90_get_var_TwoByteInt, &
                     nf90_get_var_FourByteInt, nf90_get_var_EightByteInt, &
                     nf90_get_var_FourByteReal, nf90_get_var_EightByteReal
    module procedure nf90_get_var_1D_text, &
                     nf90_get_var_1D_OneByteInt, nf90_get_var_1D_TwoByteInt, &
                     nf90_get_var_1D_FourByteInt, nf90_get_var_1D_EightByteInt, &
                     nf90_get_var_1D_FourByteReal, nf90_get_var_1D_EightByteReal
    module procedure nf90_get_var_2D_text, &
                     nf90_get_var_2D_OneByteInt, nf90_get_var_2D_TwoByteInt, &
                     nf90_get_var_2D_FourByteInt, nf90_get_var_2D_EightByteInt, &
                     nf90_get_var_2D_FourByteReal, nf90_get_var_2D_EightByteReal
    module procedure nf90_get_var_3D_text, &
                     nf90_get_var_3D_OneByteInt, nf90_get_var_3D_TwoByteInt, &
                     nf90_get_var_3D_FourByteInt, nf90_get_var_3D_EightByteInt, &
                     nf90_get_var_3D_FourByteReal, nf90_get_var_3D_EightByteReal
    module procedure nf90_get_var_4D_text, &
                     nf90_get_var_4D_OneByteInt, nf90_get_var_4D_TwoByteInt, &
                     nf90_get_var_4D_FourByteInt, nf90_get_var_4D_EightByteInt, &
                     nf90_get_var_4D_FourByteReal, nf90_get_var_4D_EightByteReal
    module procedure nf90_get_var_5D_text, &
                     nf90_get_var_5D_OneByteInt, nf90_get_var_5D_TwoByteInt, &
                     nf90_get_var_5D_FourByteInt, nf90_get_var_5D_EightByteInt, &
                     nf90_get_var_5D_FourByteReal, nf90_get_var_5D_EightByteReal
    module procedure nf90_get_var_6D_text, &
                     nf90_get_var_6D_OneByteInt, nf90_get_var_6D_TwoByteInt, &
                     nf90_get_var_6D_FourByteInt, nf90_get_var_6D_EightByteInt, &
                     nf90_get_var_6D_FourByteReal, nf90_get_var_6D_EightByteReal
    module procedure nf90_get_var_7D_text, &
                     nf90_get_var_7D_OneByteInt, nf90_get_var_7D_TwoByteInt, &
                     nf90_get_var_7D_FourByteInt, nf90_get_var_7D_EightByteInt, &
                     nf90_get_var_7D_FourByteReal, nf90_get_var_7D_EightByteReal
  end interface ! nf90_get_var
  ! Overload fill value functions
  interface nf90_def_var_fill
    module procedure nf90_def_var_fill_OneByteInt, &
                     nf90_def_var_fill_TwoByteInt, &
                     nf90_def_var_fill_FourByteInt, &
                     nf90_def_var_fill_EightByteInt, &
                     nf90_def_var_fill_FourByteReal, &
                     nf90_def_var_fill_EightByteReal
   end interface
  interface nf90_inq_var_fill
    module procedure nf90_inq_var_fill_OneByteInt, &
                     nf90_inq_var_fill_TwoByteInt, &
                     nf90_inq_var_fill_FourByteInt, &
                     nf90_inq_var_fill_EightByteInt, &
                     nf90_inq_var_fill_FourByteReal, &
                     nf90_inq_var_fill_EightByteReal
   end interface
  ! Library version, error string
  public :: nf90_inq_libvers, nf90_strerror
  ! Control routines
  public :: nf90_create, nf90_open, nf90_set_base_pe, nf90_inq_base_pe, &
            nf90_set_fill, nf90_redef, nf90_enddef, &
            nf90_create_mp, nf90_open_mp, &
            nf90_sync, nf90_abort, nf90_close, nf90_delete
  ! File level inquiry
  public :: nf90_inquire, nf90_inq_path, nf90_inq_format
  ! Dimension routines
  public :: nf90_def_dim, nf90_inq_dimid, nf90_rename_dim, nf90_inquire_dimension
  ! attribute routines
  public :: nf90_copy_att, nf90_rename_att, nf90_del_att, nf90_inq_attname, &
            nf90_inquire_attribute
  ! overloaded functions
  public :: nf90_put_att, nf90_get_att
  ! Variable routines
  public :: nf90_def_var, nf90_inq_varid, nf90_rename_var, nf90_inquire_variable
  ! overloaded functions
  public :: nf90_put_var, nf90_get_var
! This is part of the netCDF-4 fortran 90 API. Copyright 2020, UCAR
!
! This file makes visible all the public functions added for
! netCDF-4.
!
! Ed Hartnett, Dennis Heimbigner
public :: nf90_create_par, nf90_open_par, nf90_var_par_access, &
     nf90_inq_ncid, nf90_inq_grps, nf90_inq_grp_ncid, nf90_inq_grp_full_ncid, nf90_inq_grpname, &
     nf90_inq_grpname_full, nf90_inq_grpname_len, nf90_inq_varids, nf90_inq_grp_parent, &
     nf90_inq_dimids, nf90_inq_typeids, nf90_def_grp, nf90_def_compound, &
     nf90_insert_compound, nf90_insert_array_compound, nf90_inq_type, &
     nf90_inq_compound, nf90_inq_compound_name, nf90_inq_compound_size, &
     nf90_inq_compound_nfields, nf90_inq_compound_field, &
     nf90_inq_compound_fieldname, nf90_inq_compound_fieldindex, &
     nf90_inq_compound_fieldoffset, nf90_inq_compound_fieldtype, &
     nf90_inq_compound_fieldndims, nf90_inq_cmp_fielddim_sizes, nf90_def_vlen, nf90_inq_vlen, &
     nf90_def_enum, nf90_insert_enum, nf90_inq_enum, nf90_inq_enum_member, nf90_inq_enum_ident, &
     nf90_def_opaque, nf90_inq_opaque, nf90_def_var_deflate, nf90_inq_var_deflate, &
     nf90_def_var_fletcher32, nf90_inq_var_fletcher32, nf90_def_var_chunking, &
     nf90_inq_var_chunking, &
     nf90_def_var_fill, nf90_inq_var_fill, &
     nf90_def_var_endian, nf90_inq_var_endian, nf90_inq_user_type, &
     nf90_put_att_any, nf90_get_att_any, nf90_get_var_any, nf90_put_var_any, &
     nf90_rename_grp, nf90_def_var_filter, nf90_inq_var_filter, &
     nf90_def_var_szip, nf90_inq_var_szip
contains
! This is part of the netCDF F90 API, or. Copyright 2006 UCAR. See COPYRIGHT file
! for details.
! This file contains the netcdf file functions that are shared by
! netcdf-3 and netcdf-4.
! Ed Hartnett, 2010
! -------
function nf90_inq_libvers()
  character(len = 80) :: nf90_inq_libvers
  nf90_inq_libvers = nf_inq_libvers()
end function nf90_inq_libvers
! -------
function nf90_strerror(ncerr)
  integer, intent( in) :: ncerr
  character(len = 80) :: nf90_strerror
  nf90_strerror = nf_strerror(ncerr)
end function nf90_strerror
! -------
!
! File level control routines:
!
function nf90_inq_base_pe(ncid, pe)
  integer, intent( in) :: ncid
  integer, intent(out) :: pe
  integer :: nf90_inq_base_pe
  nf90_inq_base_pe = nf_inq_base_pe(ncid, pe)
end function nf90_inq_base_pe
! -------
function nf90_set_base_pe(ncid, pe)
  integer, intent( in) :: ncid, pe
  integer :: nf90_set_base_pe
  nf90_set_base_pe = nf_set_base_pe(ncid, pe)
end function nf90_set_base_pe
! -------
function nf90_create_mp(path, cmode, initalsz, basepe, chunksizehint, ncid)
  character (len = *), intent( in) :: path
  integer, intent( in) :: cmode, initalsz, basepe, chunksizehint
  integer, intent(out) :: ncid
  integer :: nf90_create_mp
  nf90_create_mp = nf__create_mp(path, cmode, initalsz, basepe, chunksizehint, ncid)
end function nf90_create_mp
! -------
function nf90_open_mp(path, mode, basepe, chunksizeint, ncid)
  character (len = *), intent( in) :: path
  integer, intent( in) :: mode, basepe, chunksizeint
  integer, intent(out) :: ncid
  integer :: nf90_open_mp
  nf90_open_mp = nf__open_mp(path, mode, basepe, chunksizeint, ncid)
end function nf90_open_mp
! -------
function nf90_set_fill(ncid, fillmode, old_mode)
  integer, intent( in) :: ncid, fillmode
  integer, intent(out) :: old_mode
  integer :: nf90_set_fill
  nf90_set_fill = nf_set_fill(ncid, fillmode, old_mode)
end function nf90_set_fill
! -------
function nf90_redef(ncid)
  integer, intent( in) :: ncid
  integer :: nf90_redef
  nf90_redef = nf_redef(ncid)
end function nf90_redef
! -------
function nf90_enddef(ncid, h_minfree, v_align, v_minfree, r_align)
  integer, intent( in) :: ncid
  integer, optional, intent( in) :: h_minfree, v_align, v_minfree, r_align
  integer :: nf90_enddef
  integer :: hMinFree, VAlign, VMinFree, RAlign
  if(.not. any( (/ present(h_minfree), present(v_align), &
       present(v_minfree), present(r_align) /) ) )then
     nf90_enddef = nf_enddef(ncid)
  else
     ! Default values per the man page
     hMinFree = 0; VMinFree = 0
     VAlign = 4; RAlign = 4
     if(present(h_minfree)) HMinFree = h_minfree
     if(present(v_align )) VAlign = v_align
     if(present(v_minfree)) VMinFree = v_minfree
     if(present(r_align )) RAlign = r_align
     nf90_enddef = nf__enddef(ncid, hMinFree, VAlign, VMinFree, RAlign)
  end if
end function nf90_enddef
! -------
function nf90_sync(ncid)
  integer, intent( in) :: ncid
  integer :: nf90_sync
  nf90_sync = nf_sync(ncid)
end function nf90_sync
! -------
function nf90_abort(ncid)
  integer, intent( in) :: ncid
  integer :: nf90_abort
  nf90_abort = nf_abort(ncid)
end function nf90_abort
! -------
function nf90_close(ncid)
  integer, intent( in) :: ncid
  integer :: nf90_close
  nf90_close = nf_close(ncid)
end function nf90_close
! -------
function nf90_delete(name)
  character(len = *), intent( in) :: name
  integer :: nf90_delete
  nf90_delete = nf_delete(name)
end function nf90_delete
!
! A single file level inquiry routine
!
function nf90_inquire(ncid, nDimensions, nVariables, nAttributes, unlimitedDimId, formatNum)
  integer, intent( in) :: ncid
  integer, optional, intent(out) :: nDimensions, nVariables, nAttributes, unlimitedDimId, formatNum
  integer :: nf90_inquire
  integer :: nDims, nVars, nGAtts, unlimDimId, frmt
  nf90_inquire = nf_inq(ncid, nDims, nVars, nGAtts, unlimDimId)
  if(present(nDimensions)) nDimensions = nDims
  if(present(nVariables)) nVariables = nVars
  if(present(nAttributes)) nAttributes = nGAtts
  if(present(unlimitedDimId)) unlimitedDimId = unlimDimId
  if(present(formatNum)) then
     nf90_inquire = nf_inq_format(ncid, frmt)
     formatNum = frmt
  endif
end function nf90_inquire
function nf90_inq_path(ncid, pathlen, path)
  integer, intent(in) :: ncid
  integer, intent(inout) :: pathlen
  character(len = *), intent(inout) :: path
  integer :: nf90_inq_path
  nf90_inq_path = nf_inq_path(ncid, pathlen, path)
end function nf90_inq_path
function nf90_inq_format(ncid, format_type)
  integer, intent(in) :: ncid
  integer, intent(out) :: format_type
  integer :: nf90_inq_format
  nf90_inq_format = nf_inq_format(ncid, format_type)
end function nf90_inq_format
! This is part of netCDF-4. Copyright 2006 UCAR. See COPYRIGHT file
! for details.
! This file contains the netcdf-4 file open and create functions.
! @author Ed Hartnett
! -------
function nf90_open(path, mode, ncid, chunksize, cache_size, cache_nelems, &
     cache_preemption, comm, info)
  implicit none
  character (len = *), intent(in) :: path
  integer, intent(in) :: mode
  integer, intent(out) :: ncid
  integer, optional, intent(inout) :: chunksize
  integer, optional, intent(in) :: cache_size, cache_nelems
  real, optional, intent(in) :: cache_preemption
  integer, optional, intent(in) :: comm, info
  integer :: size_in, nelems_in, preemption_in
  integer :: size_out, nelems_out, preemption_out, ret
  integer :: nf90_open
  ! If using parallel, both comm and info must be provided.
  if (present(comm) .and. .not. present(info)) then
     nf90_open = NF90_EINVAL;
     return
  end if
  ! If the user specified chuck cache parameters, use them. But user
  ! may have specified one, two, or three settings. Leave the others
  ! unchanged.
  if (present(cache_size) .or. present(cache_nelems) .or. &
       present(cache_preemption)) then
     ret = nf_get_chunk_cache(size_in, nelems_in, preemption_in)
     if (ret .ne. nf90_noerr) then
        nf90_open = ret
        return
     end if
     if (present(cache_size)) then
        size_out = cache_size
     else
        size_out = size_in
     end if
     if (present(cache_nelems)) then
        nelems_out = cache_nelems
     else
        nelems_out = nelems_in
     end if
     if (present(cache_preemption)) then
        preemption_out = int(cache_preemption * 100)
     else
        preemption_out = preemption_in
     end if
     nf90_open = nf_set_chunk_cache(size_out, nelems_out, preemption_out)
     if (nf90_open .ne. nf90_noerr) return
  end if
  ! Do the open.
  if(present(chunksize)) then
     nf90_open = nf__open(path, mode, chunksize, ncid)
  else
     if (present(comm)) then
        nf90_open = nf_open_par(path, mode, comm, info, ncid)
     else
        nf90_open = nf_open(path, mode, ncid)
     end if
  end if
  if (nf90_open .ne. nf90_noerr) return
  ! If settings were changed, reset chunk chache to original settings.
  if (present(cache_size) .or. present(cache_nelems) .or. &
       present(cache_preemption)) then
     nf90_open = nf_set_chunk_cache(size_in, nelems_in, preemption_in)
  end if
end function nf90_open
! -------
function nf90_create(path, cmode, ncid, initialsize, chunksize, cache_size, &
     cache_nelems, cache_preemption, comm, info)
  implicit none
  character (len = *), intent(in) :: path
  integer, intent(in) :: cmode
  integer, intent(out) :: ncid
  integer, optional, intent(in) :: initialsize
  integer, optional, intent(inout) :: chunksize
  integer, optional, intent(in) :: cache_size, cache_nelems
  integer, optional, intent(in) :: cache_preemption
  integer, optional, intent(in) :: comm, info
  integer :: size_in, nelems_in, preemption_in
  integer :: size_out, nelems_out, preemption_out
  integer :: nf90_create
  integer :: fileSize, chunk
  ! Just ignore options netCDF-3 options for netCDF-4 files, or
  ! netCDF-4 options, for netCDF-3 files, so that the same user code
  ! can work for both cases.
  ! If using parallel, but comm and info must be provided.
  if (present(comm) .and. .not. present(info)) then
     nf90_create = NF90_EINVAL;
     return
  end if
  ! If the user specified chuck cache parameters, use them. But user
  ! may have specified one, two, or three settings. Leave the others
  ! unchanged.
  if (present(cache_size) .or. present(cache_nelems) .or. &
       present(cache_preemption)) then
     nf90_create = nf_get_chunk_cache(size_in, nelems_in, preemption_in)
     if (nf90_create .ne. nf90_noerr) return
     if (present(cache_size)) then
        size_out = cache_size
     else
        size_out = size_in
     end if
     if (present(cache_nelems)) then
        nelems_out = cache_nelems
     else
        nelems_out = nelems_in
     end if
     if (present(cache_preemption)) then
        preemption_out = cache_preemption
     else
        preemption_out = preemption_in
     end if
     nf90_create = nf_set_chunk_cache(size_out, nelems_out, preemption_out)
     if (nf90_create .ne. nf90_noerr) return
  end if
  ! Do the file create.
  if(.not. (present(initialsize) .or. present(chunksize)) ) then
     if (present(comm)) then
        nf90_create = nf_create_par(path, cmode, comm, info, ncid)
     else
        nf90_create = nf_create(path, cmode, ncid)
     end if
  else
     ! Default values per man page
     filesize = 0; chunk = nf90_sizehint_default
     if(present(initialsize)) filesize = initialsize
     if(present(chunksize )) chunk = chunksize
     nf90_create = nf__create(path, cmode, filesize, chunk, ncid)
     ! Pass back the value actually used
     if(present(chunksize )) chunksize = chunk
  end if
  if (nf90_create .ne. nf90_noerr) return
  ! If settings were changed, reset chunk chache to original settings.
  if (present(cache_size) .or. present(cache_nelems) .or. &
       present(cache_preemption)) then
     nf90_create = nf_set_chunk_cache(size_in, nelems_in, preemption_in)
  end if
end function nf90_create
  !
  ! Dimension routines:
  !
  ! -----------
  function nf90_def_dim(ncid, name, len, dimid)
    integer, intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent( in) :: len
    integer, intent(out) :: dimid
    integer :: nf90_def_dim
    nf90_def_dim = nf_def_dim(ncid, name, len, dimid)
  end function nf90_def_dim
  ! -----------
  function nf90_inq_dimid(ncid, name, dimid)
    integer, intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent(out) :: dimid
    integer :: nf90_inq_dimid
    nf90_inq_dimid = nf_inq_dimid(ncid, name, dimid)
  end function nf90_inq_dimid
  ! -----------
  function nf90_rename_dim(ncid, dimid, name)
    integer, intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent( in) :: dimid
    integer :: nf90_rename_dim
    nf90_rename_dim = nf_rename_dim(ncid, dimid, name)
  end function nf90_rename_dim
  ! -----------
  function nf90_inquire_dimension(ncid, dimid, name, len)
    integer, intent( in) :: ncid, dimid
    character (len = *), optional, intent(out) :: name
    integer, optional, intent(out) :: len
    integer :: nf90_inquire_dimension
    character (len = nf90_max_name) :: dimName
    integer :: length
    nf90_inquire_dimension = nf_inq_dim(ncid, dimid, dimName, length)
    if(present(name)) name = trim(dimName)
    if(present(len )) len = length
  end function nf90_inquire_dimension
  ! -----------
  !
  ! Attribute routines:
  !
  ! -------
  function nf90_copy_att(ncid_in, varid_in, name, ncid_out, varid_out)
    integer, intent( in) :: ncid_in, varid_in
    character (len = *), intent( in) :: name
    integer, intent( in) :: ncid_out, varid_out
    integer :: nf90_copy_att
    nf90_copy_att = nf_copy_att(ncid_in, varid_in, name, ncid_out, varid_out)
  end function nf90_copy_att
  ! -------
  function nf90_rename_att(ncid, varid, curname, newname)
    integer, intent( in) :: ncid, varid
    character (len = *), intent( in) :: curname, newname
    integer :: nf90_rename_att
    nf90_rename_att = nf_rename_att(ncid, varid, curname, newname)
  end function nf90_rename_att
  ! -------
  function nf90_del_att(ncid, varid, name)
    integer, intent( in) :: ncid, varid
    character (len = *), intent( in) :: name
    integer :: nf90_del_att
    nf90_del_att = nf_del_att(ncid, varid, name)
  end function nf90_del_att
  ! -------
  ! Attribute inquiry functions
  ! -------
  function nf90_inq_attname(ncid, varid, attnum, name)
    integer, intent( in) :: ncid, varid, attnum
    character (len = *), intent(out) :: name
    integer :: nf90_inq_attname
    nf90_inq_attname = nf_inq_attname(ncid, varid, attnum, name)
  end function nf90_inq_attname
  ! -------
  function nf90_inquire_attribute(ncid, varid, name, xtype, len, attnum)
    integer, intent( in) :: ncid, varid
    character (len = *), intent( in) :: name
    integer, intent(out), optional :: xtype, len, attnum
    integer :: nf90_inquire_attribute
    integer :: local_xtype, local_len
    ! Do we need to worry about not saving the state from this call?
    if(present(attnum)) &
      nf90_inquire_attribute = nf_inq_attid(ncid, varid, name, attnum)
    nf90_inquire_attribute = nf_inq_att (ncid, varid, name, local_xtype, local_len)
    if(present(xtype)) xtype = local_xtype
    if(present(len )) len = local_len
  end function nf90_inquire_attribute
  ! -------
  ! Put and get functions; these will get overloaded
  ! -------
  ! Text
  ! -------
  function nf90_put_att_text(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    character(len = *), intent( in) :: values
    integer :: nf90_put_att_text
    nf90_put_att_text = nf_put_att_text(ncid, varid, name, len_trim(values), trim(values))
  end function nf90_put_att_text
  ! -------
  function nf90_get_att_text(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    character(len = *), intent(out) :: values
    integer :: nf90_get_att_text
    values = ' ' !! make sure result will be blank padded
    nf90_get_att_text = nf_get_att_text(ncid, varid, name, values)
  end function nf90_get_att_text
  ! -------
  ! Integer attributes
  ! -------
  function nf90_put_att_OneByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(2)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_OneByteInt
    nf90_put_att_OneByteInt = nf_put_att_int1(ncid, varid, name, nf90_int1, size(values), values)
  end function nf90_put_att_OneByteInt
  ! -------
  function nf90_put_att_one_OneByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(2)), intent( in) :: values
    integer :: nf90_put_att_one_OneByteInt
    integer (kind = selected_int_kind(2)), dimension(1) :: valuesA
    valuesA(1) = values
    nf90_put_att_one_OneByteInt = nf_put_att_int1(ncid, varid, name, nf90_int1, 1, valuesA)
  end function nf90_put_att_one_OneByteInt
  ! -------
  function nf90_get_att_OneByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(2)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_OneByteInt
    nf90_get_att_OneByteInt = nf_get_att_int1(ncid, varid, name, values)
  end function nf90_get_att_OneByteInt
  ! -------
  function nf90_get_att_one_OneByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(2)), intent(out) :: values
    integer :: nf90_get_att_one_OneByteInt
    integer (kind = selected_int_kind(2)), dimension(1) :: valuesA
    nf90_get_att_one_OneByteInt = nf_get_att_int1(ncid, varid, name, valuesA)
    values = valuesA(1)
  end function nf90_get_att_one_OneByteInt
  ! -------
  function nf90_put_att_TwoByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(4)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_TwoByteInt
    nf90_put_att_TwoByteInt = nf_put_att_int2(ncid, varid, name, nf90_int2, size(values), values)
  end function nf90_put_att_TwoByteInt
  ! -------
  function nf90_put_att_one_TwoByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(4)), intent( in) :: values
    integer :: nf90_put_att_one_TwoByteInt
    integer (kind = selected_int_kind(4)), dimension(1) :: valuesA
    valuesA(1) = values
    nf90_put_att_one_TwoByteInt = nf_put_att_int2(ncid, varid, name, nf90_int2, 1, valuesA)
  end function nf90_put_att_one_TwoByteInt
  ! -------
  function nf90_get_att_TwoByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(4)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_TwoByteInt
    nf90_get_att_TwoByteInt = nf_get_att_int2(ncid, varid, name, values)
  end function nf90_get_att_TwoByteInt
  ! -------
  function nf90_get_att_one_TwoByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(4)), intent(out) :: values
    integer :: nf90_get_att_one_TwoByteInt
    integer (kind = selected_int_kind(4)), dimension(1) :: valuesA
    nf90_get_att_one_TwoByteInt = nf_get_att_int2(ncid, varid, name, valuesA)
    values = valuesA(1)
  end function nf90_get_att_one_TwoByteInt
  ! -------
  function nf90_put_att_FourByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(9)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_FourByteInt
    nf90_put_att_FourByteInt = nf_put_att_int(ncid, varid, name, nf90_int, size(values), int(values))
  end function nf90_put_att_FourByteInt
  ! -------
  function nf90_put_att_one_FourByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(9)), intent( in) :: values
    integer :: nf90_put_att_one_FourByteInt
    integer (kind = selected_int_kind(9)), dimension(1) :: valuesA
    valuesA(1) = int(values)
    nf90_put_att_one_FourByteInt = nf_put_att_int(ncid, varid, name, nf90_int, 1, int(valuesA))
  end function nf90_put_att_one_FourByteInt
  ! -------
  function nf90_get_att_FourByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(9)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_FourByteInt
    integer, dimension(size(values)) :: defaultInteger
    nf90_get_att_FourByteInt = nf_get_att_int(ncid, varid, name, defaultInteger)
    values(:) = defaultInteger(:)
  end function nf90_get_att_FourByteInt
  ! -------
  function nf90_get_att_one_FourByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(9)), intent(out) :: values
    integer :: nf90_get_att_one_FourByteInt
    integer, dimension(1) :: defaultInteger
    nf90_get_att_one_FourByteInt = nf_get_att_int(ncid, varid, name, defaultInteger)
    values = defaultInteger(1)
  end function nf90_get_att_one_FourByteInt
  ! -------
  function nf90_put_att_EightByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(18)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_EightByteInt
    nf90_put_att_EightByteInt = nf_put_att_int64(ncid, varid, name, nf90_int64, size(values), values)
  end function nf90_put_att_EightByteInt
  ! -------
  function nf90_put_att_one_EightByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(18)), intent( in) :: values
    integer :: nf90_put_att_one_EightByteInt
    integer(kind=selected_int_kind(18)), dimension(1) :: valuesA
    valuesA(1) = values
    nf90_put_att_one_EightByteInt = nf_put_att_int64(ncid, varid, name, nf90_int64, 1, valuesA)
  end function nf90_put_att_one_EightByteInt
  ! -------
  function nf90_get_att_EightByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(18)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_EightByteInt
    nf90_get_att_EightByteInt = nf_get_att_int64(ncid, varid, name, values)
  end function nf90_get_att_EightByteInt
  ! -------
  function nf90_get_att_one_EightByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(18)), intent(out) :: values
    integer :: nf90_get_att_one_EightByteInt
    integer(kind=selected_int_kind(18)), dimension(1) :: valuesA
    nf90_get_att_one_EightByteInt = nf_get_att_int64(ncid, varid, name, valuesA)
    values = valuesA(1)
  end function nf90_get_att_one_EightByteInt
  ! -------
  ! Real attributes
  ! -------
  function nf90_put_att_FourByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  6, R =  37)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_FourByteReal
    nf90_put_att_FourByteReal = nf_put_att_real(ncid, varid, name, nf90_real4, size(values), values)
  end function nf90_put_att_FourByteReal
  ! -------
  function nf90_put_att_one_FourByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  6, R =  37)), intent( in) :: values
    integer :: nf90_put_att_one_FourByteReal
    real (kind = selected_real_kind(P =  6, R =  37)), dimension(1) :: valuesA
    valuesA(1) = values
    nf90_put_att_one_FourByteReal = nf_put_att_real(ncid, varid, name, nf90_real4, 1, valuesA)
  end function nf90_put_att_one_FourByteReal
  ! -------
  function nf90_get_att_FourByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  6, R =  37)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_FourByteReal
    nf90_get_att_FourByteReal = nf_get_att_real(ncid, varid, name, values)
  end function nf90_get_att_FourByteReal
  ! -------
  function nf90_get_att_one_FourByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  6, R =  37)), intent(out) :: values
    integer :: nf90_get_att_one_FourByteReal
    real (kind = selected_real_kind(P =  6, R =  37)), dimension(1) :: valuesA
    nf90_get_att_one_FourByteReal = nf_get_att_real(ncid, varid, name, valuesA)
    values = valuesA(1)
  end function nf90_get_att_one_FourByteReal
  ! -------
  function nf90_put_att_EightByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  13, R =  307)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_EightByteReal
    nf90_put_att_EightByteReal = nf_put_att_double(ncid, varid, name, nf90_real8, size(values), values)
  end function nf90_put_att_EightByteReal
  ! -------
  function nf90_put_att_one_EightByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  13, R =  307)), intent( in) :: values
    integer :: nf90_put_att_one_EightByteReal
    real (kind = selected_real_kind(P =  13, R =  307)), dimension(1) :: valuesA
    valuesA(1) = values
    nf90_put_att_one_EightByteReal = nf_put_att_double(ncid, varid, name, nf90_real8, 1, valuesA)
  end function nf90_put_att_one_EightByteReal
  ! -------
  function nf90_get_att_EightByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  13, R =  307)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_EightByteReal
    nf90_get_att_EightByteReal = nf_get_att_double(ncid, varid, name, values)
  end function nf90_get_att_EightByteReal
  ! -------
  function nf90_get_att_one_EightByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  13, R =  307)), intent(out) :: values
    integer :: nf90_get_att_one_EightByteReal
    real (kind = selected_real_kind(P =  13, R =  307)), dimension(1) :: valuesA
    nf90_get_att_one_EightByteReal = nf_get_att_double(ncid, varid, name, valuesA)
    values = valuesA(1)
  end function nf90_get_att_one_EightByteReal
  ! -------
  ! -----
  ! Variable definitions and inquiry
  ! -----
  function nf90_def_var_Scalar(ncid, name, xtype, varid)
    integer, intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent(in) :: xtype
    integer, intent(out) :: varid
    integer :: nf90_def_var_Scalar
    ! Dummy - shouldn't get used
    integer, dimension(1) :: dimids
    ! These may not be used with scalars, but it causes an interface
    ! violation if they are not optional arguments.
    nf90_def_var_Scalar = nf_def_var(ncid, name, xtype, 0, dimids, varid)
  end function nf90_def_var_Scalar
  ! -----
  function nf90_def_var_oneDim(ncid, name, xtype, dimids, varid, contiguous, &
       chunksizes, deflate_level, shuffle, fletcher32, endianness, &
       cache_size, cache_nelems, cache_preemption)
    integer, intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent(in) :: xtype
    integer, intent(in) :: dimids
    integer, intent(out) :: varid
    logical, optional, intent(in) :: contiguous
    integer, optional, intent(in) :: chunksizes
    integer, optional, intent(in) :: deflate_level
    logical, optional, intent(in) :: shuffle, fletcher32
    integer, optional, intent(in) :: endianness
    integer, optional, intent(in) :: cache_size, cache_nelems, cache_preemption
    integer :: nf90_def_var_oneDim
    integer, dimension(1) :: dimidsA, chunksizes1
    integer :: size1 = -1, nelems1 = -1, preemption1 = -1
    integer :: contiguous1
    ! Put this int into an array, where all decent folk keep ids.
    dimidsA(1) = dimids
    ! This is forbidden! Don't even think about it.
    if (present(contiguous)) then
       if (contiguous .and. present(chunksizes)) then
          nf90_def_var_oneDim = nf90_einval
          return
       end if
    end if
    if (present(contiguous)) then
       if (.not. contiguous .and. .not. present(chunksizes)) then
          nf90_def_var_oneDim = nf90_einval
          return
       end if
    end if
    ! Define the variable.
    nf90_def_var_oneDim = nf_def_var(ncid, name, xtype, 1, dimidsA, varid)
    if (nf90_def_var_oneDim .ne. nf90_noerr) return
    ! Handle chunksizes and contiguous.
    if (present(chunksizes) .or. present(contiguous)) then
       if (present(contiguous)) then
          if (contiguous) then
             contiguous1 = nf90_contiguous
          else
             contiguous1 = nf90_notcontiguous
          endif
       endif
       if (present(chunksizes)) then
          contiguous1 = 0
          chunksizes1(1) = chunksizes
       endif
       nf90_def_var_oneDim = nf_def_var_chunking(ncid, varid, contiguous1, chunksizes1)
    endif
    if (present(contiguous)) then
       if (contiguous) then
          chunksizes1(1) = 0
          nf90_def_var_oneDim = nf_def_var_chunking(ncid, varid, 1, chunksizes1(1:1))
       endif
    endif
    if (nf90_def_var_oneDim .ne. nf90_noerr) return
    ! Handle deflate and shuffle.
    if (present(deflate_level)) then
       if (deflate_level .gt. 0) then
          if (present(shuffle)) then
             if (shuffle) then
                nf90_def_var_oneDim = nf_def_var_deflate(ncid, varid, 1, 1, deflate_level)
             else
                nf90_def_var_oneDim = nf_def_var_deflate(ncid, varid, 0, 1, deflate_level)
             end if
             if (nf90_def_var_oneDim .ne. nf90_noerr) return
          end if
       end if
    endif
    ! Handle fletcher32.
    if (present(fletcher32)) then
       if (fletcher32) then
          nf90_def_var_oneDim = nf_def_var_fletcher32(ncid, varid, 1)
          if (nf90_def_var_oneDim .ne. nf90_noerr) return
       endif
    endif
    ! Handle endianness.
    if (present(endianness)) then
       nf90_def_var_oneDim = nf_def_var_endian(ncid, varid, endianness)
       if (nf90_def_var_oneDim .ne. nf90_noerr) return
    endif
    ! Set the cache if the user wants to.
    if (present(cache_size) .or. present(cache_nelems) .or. &
         present(cache_preemption)) then
       ! Negative values mean leave it alone.
       if (present(cache_size)) size1 = cache_size
       if (present(cache_nelems)) nelems1 = cache_nelems
       if (present(cache_preemption)) preemption1 = cache_preemption
       nf90_def_var_oneDim = nf_set_var_chunk_cache(ncid, varid, &
            size1, nelems1, preemption1)
       if (nf90_def_var_oneDim .ne. nf90_noerr) return
    endif
  end function nf90_def_var_oneDim
  ! -----
  function nf90_def_var_ManyDims(ncid, name, xtype, dimids, varid, contiguous, &
       chunksizes, deflate_level, shuffle, fletcher32, endianness, cache_size, &
       cache_nelems, cache_preemption)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: name
    integer, intent( in) :: xtype
    integer, dimension(:), intent(in) :: dimids
    integer, intent(out) :: varid
    logical, optional, intent(in) :: contiguous
    integer, optional, dimension(:), intent(in) :: chunksizes
    integer, optional, intent(in) :: deflate_level
    logical, optional, intent(in) :: shuffle, fletcher32
    integer, optional, intent(in) :: endianness
    integer, optional, intent(in) :: cache_size, cache_nelems, cache_preemption
    integer :: nf90_def_var_ManyDims
    ! Local variables.
    integer :: contiguous1, d
    integer :: size1 = -1, nelems1 = -1, preemption1 = -1
    integer, dimension(nf90_max_dims) :: chunksizes1
    ! This is forbidden!
    if (present(contiguous)) then
       if (contiguous .and. present(chunksizes)) then
          nf90_def_var_ManyDims = nf90_einval
          return
       end if
    end if
    if (present(contiguous)) then
       if (.not. contiguous .and. .not. present(chunksizes)) then
          nf90_def_var_ManyDims = nf90_einval
          return
       endif
    end if
    ! Be nice and check array size.
    if (present(chunksizes)) then
       if (size(chunksizes) .ne. size(dimids)) then
          nf90_def_var_ManyDims = nf90_einval
          return
       end if
    end if
    ! Define the variable.
    nf90_def_var_ManyDims = nf_def_var(ncid, name, xtype, size(dimids), dimids, varid)
    if (nf90_def_var_ManyDims .ne. nf90_noerr) return
    ! Handle chunksizes and contiguous.
    if (present(chunksizes) .or. present(contiguous)) then
       if (present(contiguous)) then
          if (contiguous) then
             contiguous1 = nf90_contiguous
          else
             contiguous1 = nf90_notcontiguous
          endif
       endif
       if (present(chunksizes)) then
          contiguous1 = 0
          do d = 1, size(dimids)
             chunksizes1(d) = chunksizes(d)
          end do
       endif
       nf90_def_var_ManyDims = nf_def_var_chunking(ncid, varid, contiguous1, chunksizes1)
    endif
    if (present(contiguous)) then
       if (contiguous) then
          chunksizes1(1) = 0
          nf90_def_var_ManyDims = nf_def_var_chunking(ncid, varid, 1, chunksizes1(1:1))
       endif
    endif
    if (nf90_def_var_ManyDims .ne. nf90_noerr) return
    ! Handle deflate and shuffle.
    if (present(deflate_level)) then
       if (deflate_level .gt. 0) then
          if (present(shuffle)) then
             if (shuffle) then
                nf90_def_var_ManyDims = nf_def_var_deflate(ncid, varid, 1, 1, deflate_level)
             else
                nf90_def_var_ManyDims = nf_def_var_deflate(ncid, varid, 0, 1, deflate_level)
             end if
          else
             nf90_def_var_ManyDims = nf_def_var_deflate(ncid, varid, 0, 1, deflate_level)
          end if
       end if
    endif
    if (nf90_def_var_ManyDims .ne. nf90_noerr) return
    ! Handle fletcher32.
    if (present(fletcher32)) then
       if (fletcher32) then
          nf90_def_var_ManyDims = nf_def_var_fletcher32(ncid, varid, 1)
       endif
    endif
    if (nf90_def_var_ManyDims .ne. nf90_noerr) return
    ! Handle endianness.
    if (present(endianness)) then
       nf90_def_var_ManyDims = nf_def_var_endian(ncid, varid, endianness)
    endif
    ! Set the cache if the user wants to.
    if (present(cache_size) .or. present(cache_nelems) .or. &
         present(cache_preemption)) then
       ! Negative values mean leave it alone.
       if (present(cache_size)) size1 = cache_size
       if (present(cache_nelems)) nelems1 = cache_nelems
       if (present(cache_preemption)) preemption1 = cache_preemption
       nf90_def_var_ManyDims = nf_set_var_chunk_cache(ncid, varid, &
            size1, nelems1, preemption1)
       if (nf90_def_var_ManyDims .ne. nf90_noerr) return
    endif
  end function nf90_def_var_ManyDims
  ! -----
  function nf90_inq_varid(ncid, name, varid)
    integer, intent(in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent(out) :: varid
    integer :: nf90_inq_varid
    nf90_inq_varid = nf_inq_varid(ncid, name, varid)
    if (nf90_inq_varid .ne. nf90_noerr) return
  end function nf90_inq_varid
  ! -----
  function nf90_set_var_chunk_cache(ncid, varid, size, nelems, preemption)
    integer, intent(in) :: ncid, varid, size, nelems, preemption
    integer :: nf90_set_var_chunk_cache
    nf90_set_var_chunk_cache = nf_set_var_chunk_cache(ncid, varid, &
         size, nelems, preemption)
    if (nf90_set_var_chunk_cache .ne. nf90_noerr) return
  end function nf90_set_var_chunk_cache
  ! -----
  function nf90_inquire_variable(ncid, varid, name, xtype, ndims, dimids, nAtts, &
       contiguous, chunksizes, deflate_level, shuffle, fletcher32, endianness, &
       cache_size, cache_nelems, cache_preemption)
    integer, intent(in) :: ncid, varid
    character (len = *), optional, intent(out) :: name
    integer, optional, intent(out) :: xtype, ndims
    integer, dimension(:), optional, intent(out) :: dimids
    integer, optional, intent(out) :: nAtts
    logical, optional, intent(out) :: contiguous
    integer, optional, dimension(:), intent(out) :: chunksizes
    integer, optional, intent(out) :: deflate_level
    logical, optional, intent(out) :: shuffle, fletcher32
    integer, optional, intent(out) :: endianness
    integer, optional, intent(out) :: cache_size, cache_nelems, cache_preemption
    integer :: nf90_inquire_variable
    ! Local variables
    character (len = nf90_max_name) :: varName = ''
    integer :: externalType, numDimensions
    integer, dimension(nf90_max_var_dims) :: dimensionIDs
    integer :: numAttributes
    integer :: deflate1, deflate_level1, contiguous1, shuffle1, fletcher321
    integer, dimension(nf90_max_dims) :: chunksizes1
    integer :: size1, nelems1, preemption1
    integer :: d
    ! Learn the basic facts.
    nf90_inquire_variable = nf_inq_var(ncid, varid, varName, externalType, &
                                       numDimensions, dimensionIDs, numAttributes)
    if (nf90_inquire_variable .ne. nf90_noerr) return
    ! Tell the user what he wants to know.
    if (present(name)) name = trim(varName)
    if (present(xtype)) xtype = externalType
    if (present(ndims)) ndims = numDimensions
    if (present(dimids)) then
       if (size(dimids) .ge. numDimensions) then
          dimids(:numDimensions) = dimensionIDs(:numDimensions)
       else
          nf90_inquire_variable = nf90_einval
       endif
    endif
    if (present(nAtts)) nAtts = numAttributes
    ! Get the chunksizes and contiguous settings, if desired.
    if (present(chunksizes) .or. present(contiguous)) then
       nf90_inquire_variable = nf_inq_var_chunking(ncid, varid, contiguous1, chunksizes1)
       if (nf90_inquire_variable .ne. nf90_noerr) return
       if (present(contiguous)) contiguous = contiguous1 .ne. nf90_notcontiguous
       if (present(chunksizes)) then
          do d = 1, numDimensions
             chunksizes(d) = chunksizes1(d)
          end do
       endif
    endif
    ! Get the fletcher32 settings, if desired.
    if (present(fletcher32)) then
       nf90_inquire_variable = nf_inq_var_fletcher32(ncid, varid, fletcher321)
       if (nf90_inquire_variable .ne. nf90_noerr) return
       fletcher32 = fletcher321 .gt. 0
    endif
    ! Get the deflate and shuffle settings, if desired.
    if (present(deflate_level) .or. present(shuffle)) then
       nf90_inquire_variable = nf_inq_var_deflate(ncid, varid, shuffle1, deflate1, deflate_level1)
       if (nf90_inquire_variable .ne. nf90_noerr) return
       if (present(deflate_level)) deflate_level = deflate_level1
       if (present(shuffle)) shuffle = shuffle1 .ne. 0
    endif
    ! And the endianness...
    if (present(endianness)) then
       nf90_inquire_variable = nf_inq_var_endian(ncid, varid, endianness)
       if (nf90_inquire_variable .ne. nf90_noerr) return
    endif
    ! Does the user want cache settings?
    if (present(cache_size) .or. present(cache_nelems) .or. present(cache_preemption)) then
       nf90_inquire_variable = nf_get_var_chunk_cache(ncid, varid, &
            size1, nelems1, preemption1)
       if (nf90_inquire_variable .ne. nf90_noerr) return
       if (present(cache_size)) cache_size = size1
       if (present(cache_nelems)) cache_nelems = nelems1
       if (present(cache_preemption)) cache_preemption = preemption1
    endif
  end function nf90_inquire_variable
  ! -----
  function nf90_rename_var(ncid, varid, newname)
    integer, intent( in) :: ncid, varid
    character (len = *), intent( in) :: newname
    integer :: nf90_rename_var
    nf90_rename_var = nf_rename_var(ncid, varid, newname)
  end function nf90_rename_var
  ! -----
   function nf90_put_var_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_text
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride
     ! Set local arguments to default values
     localStart (:) = 1
     localCount (1) = len(values); localCount (2:) = 1
     localStride(:) = 1
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     nf90_put_var_text = nf_put_vars_text(ncid, varid, localStart, localCount, localStride, values)
   end function nf90_put_var_text
   function nf90_get_var_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_text
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride
     ! Set local arguments to default values
     localStart (:) = 1
     localCount (1) = len(values); localCount (2:) = 1
     localStride(:) = 1
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     nf90_get_var_text = nf_get_vars_text(ncid, varid, localStart, localCount, localStride, values)
   end function nf90_get_var_text
   function nf90_put_var_1D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_text
     integer, parameter :: numDims = 1
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount ( :numDims+1) = (/ len(values(1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_1D_text = &
          nf_put_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1))
     else
       nf90_put_var_1D_text = &
          nf_put_vars_text(ncid, varid, localStart, localCount, localStride, values(1))
     end if
   end function nf90_put_var_1D_text
   function nf90_put_var_2D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_text
     integer, parameter :: numDims = 2
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount ( :numDims+1) = (/ len(values(1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_2D_text = &
          nf_put_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1))
     else
       nf90_put_var_2D_text = &
          nf_put_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1))
     end if
   end function nf90_put_var_2D_text
   function nf90_put_var_3D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_text
     integer, parameter :: numDims = 3
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount ( :numDims+1) = (/ len(values(1, 1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_3D_text = &
          nf_put_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1,1))
     else
       nf90_put_var_3D_text = &
          nf_put_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1,1))
     end if
   end function nf90_put_var_3D_text
   function nf90_put_var_4D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_text
     integer, parameter :: numDims = 4
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount ( :numDims+1) = (/ len(values(1, 1, 1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_4D_text = &
          nf_put_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1,1,1))
     else
       nf90_put_var_4D_text = &
          nf_put_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1,1,1))
     end if
   end function nf90_put_var_4D_text
   function nf90_put_var_5D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_text
     integer, parameter :: numDims = 5
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount ( :numDims+1) = (/ len(values(1, 1, 1, 1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_5D_text = &
          nf_put_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1,1,1,1))
     else
       nf90_put_var_5D_text = &
          nf_put_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1,1,1,1))
     end if
   end function nf90_put_var_5D_text
   function nf90_put_var_6D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_text
     integer, parameter :: numDims = 6
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount ( :numDims+1) = (/ len(values(1, 1, 1, 1, 1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_6D_text = &
          nf_put_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1,1,1,1,1))
     else
       nf90_put_var_6D_text = &
          nf_put_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1,1,1,1,1))
     end if
   end function nf90_put_var_6D_text
   function nf90_put_var_7D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_text
     integer, parameter :: numDims = 7
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount ( :numDims+1) = (/ len(values(1, 1, 1, 1, 1, 1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_7D_text = &
          nf_put_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1,1,1,1,1,1))
     else
       nf90_put_var_7D_text = &
          nf_put_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1,1,1,1,1,1))
     end if
   end function nf90_put_var_7D_text
   function nf90_get_var_1D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_text
     integer, parameter :: numDims = 1
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount (:numDims+1) = (/ len(values(1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1/)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_1D_text = &
          nf_get_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1))
     else
       nf90_get_var_1D_text = &
          nf_get_vars_text(ncid, varid, localStart, localCount, localStride, values(1))
     end if
   end function nf90_get_var_1D_text
   function nf90_get_var_2D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_text
     integer, parameter :: numDims = 2
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount (:numDims+1) = (/ len(values(1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_2D_text = &
          nf_get_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1))
     else
       nf90_get_var_2D_text = &
          nf_get_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1))
     end if
   end function nf90_get_var_2D_text
   function nf90_get_var_3D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_text
     integer, parameter :: numDims = 3
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount (:numDims+1) = (/ len(values(1, 1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_3D_text = &
          nf_get_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1,1))
     else
       nf90_get_var_3D_text = &
          nf_get_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1,1))
     end if
   end function nf90_get_var_3D_text
   function nf90_get_var_4D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_text
     integer, parameter :: numDims = 4
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount (:numDims+1) = (/ len(values(1, 1, 1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_4D_text = &
          nf_get_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1,1,1))
     else
       nf90_get_var_4D_text = &
          nf_get_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1,1,1))
     end if
   end function nf90_get_var_4D_text
   function nf90_get_var_5D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_text
     integer, parameter :: numDims = 5
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount (:numDims+1) = (/ len(values(1, 1, 1, 1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_5D_text = &
          nf_get_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1,1,1,1))
     else
       nf90_get_var_5D_text = &
          nf_get_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1,1,1,1))
     end if
   end function nf90_get_var_5D_text
   function nf90_get_var_6D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_text
     integer, parameter :: numDims = 6
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount (:numDims+1) = (/ len(values(1, 1, 1, 1, 1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_6D_text = &
          nf_get_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1,1,1,1,1))
     else
       nf90_get_var_6D_text = &
          nf_get_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1,1,1,1,1))
     end if
   end function nf90_get_var_6D_text
   function nf90_get_var_7D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_text
     integer, parameter :: numDims = 7
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: counter
     ! Set local arguments to default values
     localStart (: ) = 1
     localCount (:numDims+1) = (/ len(values(1, 1, 1, 1, 1, 1, 1)), shape(values) /)
     localCount (numDims+2:) = 0
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start)) = start(:)
     if(present(count)) localCount (:size(count)) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_7D_text = &
          nf_get_varm_text(ncid, varid, localStart, localCount, localStride, localMap, values(1,1,1,1,1,1,1))
     else
       nf90_get_var_7D_text = &
          nf_get_vars_text(ncid, varid, localStart, localCount, localStride, values(1,1,1,1,1,1,1))
     end if
   end function nf90_get_var_7D_text
   function nf90_put_var_OneByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_put_var_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localIndex
     ! Set local arguments to default values
     localIndex(:) = 1
     if(present(start)) localIndex(:size(start)) = start(:)
     nf90_put_var_OneByteInt = nf_put_var1_int1(ncid, varid, localIndex, values)
   end function nf90_put_var_OneByteInt
   function nf90_put_var_TwoByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_put_var_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localIndex
     ! Set local arguments to default values
     localIndex(:) = 1
     if(present(start)) localIndex(:size(start)) = start(:)
     nf90_put_var_TwoByteInt = nf_put_var1_int2(ncid, varid, localIndex, values)
   end function nf90_put_var_TwoByteInt
   function nf90_put_var_FourByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_put_var_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localIndex
     ! Set local arguments to default values
     localIndex(:) = 1
     if(present(start)) localIndex(:size(start)) = start(:)
     nf90_put_var_FourByteInt = nf_put_var1_int(ncid, varid, localIndex, int(values))
   end function nf90_put_var_FourByteInt
   function nf90_put_var_FourByteReal(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_put_var_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localIndex
     ! Set local arguments to default values
     localIndex(:) = 1
     if(present(start)) localIndex(:size(start)) = start(:)
     nf90_put_var_FourByteReal = nf_put_var1_real(ncid, varid, localIndex, values)
   end function nf90_put_var_FourByteReal
   function nf90_put_var_EightByteReal(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_put_var_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localIndex
     ! Set local arguments to default values
     localIndex(:) = 1
     if(present(start)) localIndex(:size(start)) = start(:)
     nf90_put_var_EightByteReal = nf_put_var1_double(ncid, varid, localIndex, values)
   end function nf90_put_var_EightByteReal
   function nf90_get_var_OneByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_get_var_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localIndex
     ! Set local arguments to default values
     localIndex(:) = 1
     if(present(start)) localIndex(:size(start)) = start(:)
     nf90_get_var_OneByteInt = nf_get_var1_int1(ncid, varid, localIndex, values)
   end function nf90_get_var_OneByteInt
   function nf90_get_var_TwoByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_get_var_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localIndex
     ! Set local arguments to default values
     localIndex(:) = 1
     if(present(start)) localIndex(:size(start)) = start(:)
     nf90_get_var_TwoByteInt = nf_get_var1_int2(ncid, varid, localIndex, values)
   end function nf90_get_var_TwoByteInt
   function nf90_get_var_FourByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_get_var_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localIndex
     integer :: defaultInteger
     ! Set local arguments to default values
     localIndex(:) = 1
     if(present(start)) localIndex(:size(start)) = start(:)
     nf90_get_var_FourByteInt = nf_get_var1_int(ncid, varid, localIndex, defaultInteger)
     values = defaultInteger
   end function nf90_get_var_FourByteInt
   function nf90_get_var_FourByteReal(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_get_var_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localIndex
     ! Set local arguments to default values
     localIndex(:) = 1
     if(present(start)) localIndex(:size(start)) = start(:)
     nf90_get_var_FourByteReal = nf_get_var1_real(ncid, varid, localIndex, values)
   end function nf90_get_var_FourByteReal
   function nf90_get_var_EightByteReal(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_get_var_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localIndex
     ! Set local arguments to default values
     localIndex(:) = 1
     if(present(start)) localIndex(:size(start)) = start(:)
     nf90_get_var_EightByteReal = nf_get_var1_double(ncid, varid, localIndex, values)
   end function nf90_get_var_EightByteReal
   function nf90_put_var_1D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_1D_OneByteInt = &
          nf_put_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_1D_OneByteInt = &
          nf_put_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_1D_OneByteInt = &
          nf_put_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_1D_OneByteInt
   function nf90_put_var_2D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_2D_OneByteInt = &
          nf_put_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_2D_OneByteInt = &
          nf_put_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_2D_OneByteInt = &
          nf_put_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_2D_OneByteInt
   function nf90_put_var_3D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_3D_OneByteInt = &
          nf_put_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_3D_OneByteInt = &
          nf_put_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_3D_OneByteInt = &
          nf_put_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_3D_OneByteInt
   function nf90_put_var_4D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_4D_OneByteInt = &
          nf_put_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_4D_OneByteInt = &
          nf_put_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_4D_OneByteInt = &
          nf_put_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_4D_OneByteInt
   function nf90_put_var_5D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_5D_OneByteInt = &
          nf_put_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_5D_OneByteInt = &
          nf_put_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_5D_OneByteInt = &
          nf_put_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_5D_OneByteInt
   function nf90_put_var_6D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_6D_OneByteInt = &
          nf_put_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_6D_OneByteInt = &
          nf_put_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_6D_OneByteInt = &
          nf_put_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_6D_OneByteInt
   function nf90_put_var_7D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_7D_OneByteInt = &
          nf_put_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_7D_OneByteInt = &
          nf_put_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_7D_OneByteInt = &
          nf_put_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_7D_OneByteInt
   function nf90_put_var_1D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_1D_TwoByteInt = &
          nf_put_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_1D_TwoByteInt = &
          nf_put_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_1D_TwoByteInt = &
          nf_put_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_1D_TwoByteInt
   function nf90_put_var_2D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_2D_TwoByteInt = &
          nf_put_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_2D_TwoByteInt = &
          nf_put_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_2D_TwoByteInt = &
          nf_put_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_2D_TwoByteInt
   function nf90_put_var_3D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_3D_TwoByteInt = &
          nf_put_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_3D_TwoByteInt = &
          nf_put_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_3D_TwoByteInt = &
          nf_put_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_3D_TwoByteInt
   function nf90_put_var_4D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_4D_TwoByteInt = &
          nf_put_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_4D_TwoByteInt = &
          nf_put_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_4D_TwoByteInt = &
          nf_put_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_4D_TwoByteInt
   function nf90_put_var_5D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_5D_TwoByteInt = &
          nf_put_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_5D_TwoByteInt = &
          nf_put_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_5D_TwoByteInt = &
          nf_put_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_5D_TwoByteInt
   function nf90_put_var_6D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_6D_TwoByteInt = &
          nf_put_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_6D_TwoByteInt = &
          nf_put_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_6D_TwoByteInt = &
          nf_put_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_6D_TwoByteInt
   function nf90_put_var_7D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_7D_TwoByteInt = &
          nf_put_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_7D_TwoByteInt = &
          nf_put_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_7D_TwoByteInt = &
          nf_put_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_7D_TwoByteInt
   function nf90_put_var_1D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_1D_FourByteInt = &
          nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
     else if(present(stride)) then
       nf90_put_var_1D_FourByteInt = &
          nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
     else
       nf90_put_var_1D_FourByteInt = &
          nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
     end if
   end function nf90_put_var_1D_FourByteInt
   function nf90_put_var_2D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
        localMap (:size(map)) = map(:)
        nf90_put_var_2D_FourByteInt = &
             nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
     else if(present(stride)) then
        nf90_put_var_2D_FourByteInt = &
             nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
     else
        nf90_put_var_2D_FourByteInt = &
             nf_put_vara_int(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_2D_FourByteInt
   function nf90_put_var_3D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_3D_FourByteInt = &
          nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
     else if(present(stride)) then
       nf90_put_var_3D_FourByteInt = &
          nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
     else
       nf90_put_var_3D_FourByteInt = &
          nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
     end if
   end function nf90_put_var_3D_FourByteInt
   function nf90_put_var_4D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_4D_FourByteInt = &
          nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
     else if(present(stride)) then
       nf90_put_var_4D_FourByteInt = &
          nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
     else
       nf90_put_var_4D_FourByteInt = &
          nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
     end if
   end function nf90_put_var_4D_FourByteInt
   function nf90_put_var_5D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_5D_FourByteInt = &
          nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
     else if(present(stride)) then
       nf90_put_var_5D_FourByteInt = &
          nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
     else
       nf90_put_var_5D_FourByteInt = &
          nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
     end if
   end function nf90_put_var_5D_FourByteInt
   function nf90_put_var_6D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_6D_FourByteInt = &
          nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
     else if(present(stride)) then
       nf90_put_var_6D_FourByteInt = &
          nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
     else
       nf90_put_var_6D_FourByteInt = &
          nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
     end if
   end function nf90_put_var_6D_FourByteInt
   function nf90_put_var_7D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_7D_FourByteInt = &
          nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
     else if(present(stride)) then
       nf90_put_var_7D_FourByteInt = &
          nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
     else
       nf90_put_var_7D_FourByteInt = &
          nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
     end if
   end function nf90_put_var_7D_FourByteInt
    function nf90_put_var_1D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_1D_FourByteReal = &
          nf_put_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_1D_FourByteReal = &
          nf_put_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_1D_FourByteReal = &
          nf_put_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_1D_FourByteReal
   function nf90_put_var_2D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_2D_FourByteReal = &
          nf_put_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_2D_FourByteReal = &
          nf_put_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_2D_FourByteReal = &
          nf_put_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_2D_FourByteReal
   function nf90_put_var_3D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_3D_FourByteReal = &
          nf_put_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_3D_FourByteReal = &
          nf_put_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_3D_FourByteReal = &
          nf_put_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_3D_FourByteReal
   function nf90_put_var_4D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_4D_FourByteReal = &
          nf_put_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_4D_FourByteReal = &
          nf_put_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_4D_FourByteReal = &
          nf_put_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_4D_FourByteReal
   function nf90_put_var_5D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_5D_FourByteReal = &
          nf_put_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_5D_FourByteReal = &
          nf_put_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_5D_FourByteReal = &
          nf_put_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_5D_FourByteReal
   function nf90_put_var_6D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_6D_FourByteReal = &
          nf_put_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_6D_FourByteReal = &
          nf_put_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_6D_FourByteReal = &
          nf_put_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_6D_FourByteReal
   function nf90_put_var_7D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_7D_FourByteReal = &
          nf_put_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_7D_FourByteReal = &
          nf_put_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_7D_FourByteReal = &
          nf_put_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_7D_FourByteReal
   function nf90_put_var_1D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_1D_EightByteReal = &
          nf_put_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_1D_EightByteReal = &
          nf_put_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_1D_EightByteReal = &
          nf_put_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_1D_EightByteReal
   function nf90_put_var_2D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_2D_EightByteReal = &
          nf_put_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_2D_EightByteReal = &
          nf_put_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_2D_EightByteReal = &
          nf_put_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_2D_EightByteReal
   function nf90_put_var_3D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_3D_EightByteReal = &
          nf_put_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_3D_EightByteReal = &
          nf_put_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_3D_EightByteReal = &
          nf_put_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_3D_EightByteReal
   function nf90_put_var_4D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_4D_EightByteReal = &
          nf_put_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_4D_EightByteReal = &
          nf_put_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_4D_EightByteReal = &
          nf_put_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_4D_EightByteReal
   function nf90_put_var_5D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_5D_EightByteReal = &
          nf_put_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_5D_EightByteReal = &
          nf_put_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_5D_EightByteReal = &
          nf_put_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_5D_EightByteReal
   function nf90_put_var_6D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_6D_EightByteReal = &
          nf_put_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_6D_EightByteReal = &
          nf_put_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_6D_EightByteReal = &
          nf_put_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_6D_EightByteReal
   function nf90_put_var_7D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_put_var_7D_EightByteReal = &
          nf_put_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_put_var_7D_EightByteReal = &
          nf_put_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_put_var_7D_EightByteReal = &
          nf_put_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_put_var_7D_EightByteReal
   function nf90_get_var_1D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_1D_OneByteInt = &
          nf_get_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_1D_OneByteInt = &
          nf_get_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_1D_OneByteInt = &
          nf_get_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_1D_OneByteInt
   function nf90_get_var_2D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_2D_OneByteInt = &
          nf_get_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_2D_OneByteInt = &
          nf_get_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_2D_OneByteInt = &
          nf_get_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_2D_OneByteInt
   function nf90_get_var_3D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_3D_OneByteInt = &
          nf_get_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_3D_OneByteInt = &
          nf_get_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_3D_OneByteInt = &
          nf_get_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_3D_OneByteInt
   function nf90_get_var_4D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_4D_OneByteInt = &
          nf_get_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_4D_OneByteInt = &
          nf_get_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_4D_OneByteInt = &
          nf_get_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_4D_OneByteInt
   function nf90_get_var_5D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_5D_OneByteInt = &
          nf_get_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_5D_OneByteInt = &
          nf_get_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_5D_OneByteInt = &
          nf_get_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_5D_OneByteInt
   function nf90_get_var_6D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_6D_OneByteInt = &
          nf_get_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_6D_OneByteInt = &
          nf_get_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_6D_OneByteInt = &
          nf_get_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_6D_OneByteInt
   function nf90_get_var_7D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_OneByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_7D_OneByteInt = &
          nf_get_varm_int1(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_7D_OneByteInt = &
          nf_get_vars_int1(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_7D_OneByteInt = &
          nf_get_vara_int1(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_7D_OneByteInt
   function nf90_get_var_1D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_1D_TwoByteInt = &
          nf_get_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_1D_TwoByteInt = &
          nf_get_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_1D_TwoByteInt = &
          nf_get_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_1D_TwoByteInt
   function nf90_get_var_2D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_2D_TwoByteInt = &
          nf_get_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_2D_TwoByteInt = &
          nf_get_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_2D_TwoByteInt = &
          nf_get_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_2D_TwoByteInt
   function nf90_get_var_3D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_3D_TwoByteInt = &
          nf_get_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_3D_TwoByteInt = &
          nf_get_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_3D_TwoByteInt = &
          nf_get_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_3D_TwoByteInt
   function nf90_get_var_4D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_4D_TwoByteInt = &
          nf_get_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_4D_TwoByteInt = &
          nf_get_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_4D_TwoByteInt = &
          nf_get_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_4D_TwoByteInt
   function nf90_get_var_5D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_5D_TwoByteInt = &
          nf_get_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_5D_TwoByteInt = &
          nf_get_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_5D_TwoByteInt = &
          nf_get_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_5D_TwoByteInt
   function nf90_get_var_6D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_6D_TwoByteInt = &
          nf_get_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_6D_TwoByteInt = &
          nf_get_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_6D_TwoByteInt = &
          nf_get_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_6D_TwoByteInt
   function nf90_get_var_7D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_TwoByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_7D_TwoByteInt = &
          nf_get_varm_int2(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_7D_TwoByteInt = &
          nf_get_vars_int2(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_7D_TwoByteInt = &
          nf_get_vara_int2(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_7D_TwoByteInt
   function nf90_get_var_1D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     integer, dimension(size(values)) :: defaultIntArray
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_1D_FourByteInt = &
          nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
     else if(present(stride)) then
       nf90_get_var_1D_FourByteInt = &
          nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
     else
       nf90_get_var_1D_FourByteInt = &
          nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
     end if
     values(:) = reshape(defaultIntArray(:), shape(values))
   end function nf90_get_var_1D_FourByteInt
   function nf90_get_var_2D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     integer, dimension(size(values)) :: defaultIntArray
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_2D_FourByteInt = &
          nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
     else if(present(stride)) then
       nf90_get_var_2D_FourByteInt = &
          nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
     else
       nf90_get_var_2D_FourByteInt = &
          nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
     end if
     values(:, :) = reshape(defaultIntArray(:), shape(values))
   end function nf90_get_var_2D_FourByteInt
   function nf90_get_var_3D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     integer, dimension(size(values)) :: defaultIntArray
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_3D_FourByteInt = &
          nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
     else if(present(stride)) then
       nf90_get_var_3D_FourByteInt = &
          nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
     else
       nf90_get_var_3D_FourByteInt = &
          nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
     end if
     values(:, :, :) = reshape(defaultIntArray(:), shape(values))
   end function nf90_get_var_3D_FourByteInt
   function nf90_get_var_4D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     integer, dimension(size(values)) :: defaultIntArray
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_4D_FourByteInt = &
          nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
     else if(present(stride)) then
       nf90_get_var_4D_FourByteInt = &
          nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
     else
       nf90_get_var_4D_FourByteInt = &
          nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
     end if
     values(:, :, :, :) = reshape(defaultIntArray(:), shape(values))
   end function nf90_get_var_4D_FourByteInt
   function nf90_get_var_5D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     integer, dimension(size(values)) :: defaultIntArray
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_5D_FourByteInt = &
          nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
     else if(present(stride)) then
       nf90_get_var_5D_FourByteInt = &
          nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
     else
       nf90_get_var_5D_FourByteInt = &
          nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
     end if
     values(:, :, :, :, :) = reshape(defaultIntArray(:), shape(values))
   end function nf90_get_var_5D_FourByteInt
   function nf90_get_var_6D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     integer, dimension(size(values)) :: defaultIntArray
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_6D_FourByteInt = &
          nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
     else if(present(stride)) then
       nf90_get_var_6D_FourByteInt = &
          nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
     else
       nf90_get_var_6D_FourByteInt = &
          nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
     end if
     values(:, :, :, :, :, :) = reshape(defaultIntArray(:), shape(values))
   end function nf90_get_var_6D_FourByteInt
   function nf90_get_var_7D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_FourByteInt
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     integer, dimension(size(values)) :: defaultIntArray
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_7D_FourByteInt = &
          nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
     else if(present(stride)) then
       nf90_get_var_7D_FourByteInt = &
          nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
     else
       nf90_get_var_7D_FourByteInt = &
          nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
     end if
     values(:, :, :, :, :, :, :) = reshape(defaultIntArray(:), shape(values))
   end function nf90_get_var_7D_FourByteInt
   function nf90_get_var_1D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_1D_FourByteReal = &
          nf_get_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_1D_FourByteReal = &
          nf_get_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_1D_FourByteReal = &
          nf_get_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_1D_FourByteReal
   function nf90_get_var_2D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_2D_FourByteReal = &
          nf_get_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_2D_FourByteReal = &
          nf_get_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_2D_FourByteReal = &
          nf_get_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_2D_FourByteReal
   function nf90_get_var_3D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_3D_FourByteReal = &
          nf_get_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_3D_FourByteReal = &
          nf_get_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_3D_FourByteReal = &
          nf_get_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_3D_FourByteReal
   function nf90_get_var_4D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_4D_FourByteReal = &
          nf_get_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_4D_FourByteReal = &
          nf_get_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_4D_FourByteReal = &
          nf_get_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_4D_FourByteReal
   function nf90_get_var_5D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_5D_FourByteReal = &
          nf_get_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_5D_FourByteReal = &
          nf_get_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_5D_FourByteReal = &
          nf_get_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_5D_FourByteReal
   function nf90_get_var_6D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_6D_FourByteReal = &
          nf_get_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_6D_FourByteReal = &
          nf_get_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_6D_FourByteReal = &
          nf_get_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_6D_FourByteReal
   function nf90_get_var_7D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_FourByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_7D_FourByteReal = &
          nf_get_varm_real(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_7D_FourByteReal = &
          nf_get_vars_real(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_7D_FourByteReal = &
          nf_get_vara_real(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_7D_FourByteReal
   function nf90_get_var_1D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_1D_EightByteReal = &
          nf_get_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_1D_EightByteReal = &
          nf_get_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_1D_EightByteReal = &
          nf_get_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_1D_EightByteReal
   function nf90_get_var_2D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_2D_EightByteReal = &
          nf_get_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_2D_EightByteReal = &
          nf_get_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_2D_EightByteReal = &
          nf_get_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_2D_EightByteReal
   function nf90_get_var_3D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_3D_EightByteReal = &
          nf_get_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_3D_EightByteReal = &
          nf_get_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_3D_EightByteReal = &
          nf_get_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_3D_EightByteReal
   function nf90_get_var_4D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_4D_EightByteReal = &
          nf_get_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_4D_EightByteReal = &
          nf_get_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_4D_EightByteReal = &
          nf_get_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_4D_EightByteReal
   function nf90_get_var_5D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_5D_EightByteReal = &
          nf_get_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_5D_EightByteReal = &
          nf_get_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_5D_EightByteReal = &
          nf_get_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_5D_EightByteReal
   function nf90_get_var_6D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_6D_EightByteReal = &
          nf_get_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_6D_EightByteReal = &
          nf_get_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_6D_EightByteReal = &
          nf_get_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_6D_EightByteReal
   function nf90_get_var_7D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_EightByteReal
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
     integer :: numDims, counter
     ! Set local arguments to default values
     numDims = size(shape(values))
     localStart (: ) = 1
     localCount (:numDims ) = shape(values)
     localCount (numDims+1:) = 1
     localStride(: ) = 1
     localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     if(present(map)) then
       localMap (:size(map)) = map(:)
       nf90_get_var_7D_EightByteReal = &
          nf_get_varm_double(ncid, varid, localStart, localCount, localStride, localMap, values)
     else if(present(stride)) then
       nf90_get_var_7D_EightByteReal = &
          nf_get_vars_double(ncid, varid, localStart, localCount, localStride, values)
     else
       nf90_get_var_7D_EightByteReal = &
          nf_get_vara_double(ncid, varid, localStart, localCount, values)
     end if
   end function nf90_get_var_7D_EightByteReal
function nf90_put_var_1D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:), intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_1D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_put_var_1D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_put_var_1D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_1D_EightByteInt = &
                nf_put_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, values)
        else if(present(stride)) then
           nf90_put_var_1D_EightByteInt = &
                nf_put_vars_int64(ncid, varid, localStart, localCount, localStride, values)
        else
           nf90_put_var_1D_EightByteInt = &
                nf_put_vara_int64(ncid, varid, localStart, localCount, values)
        end if
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_1D_EightByteInt = &
                nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
        else if(present(stride)) then
           nf90_put_var_1D_EightByteInt = &
                nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
        else
           nf90_put_var_1D_EightByteInt = &
                nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
        end if
     endif
  endif
end function nf90_put_var_1D_EightByteInt
function nf90_put_var_2D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_2D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_put_var_2D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_put_var_2D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_2D_EightByteInt = &
                nf_put_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, values)
        else if(present(stride)) then
           nf90_put_var_2D_EightByteInt = &
                nf_put_vars_int64(ncid, varid, localStart, localCount, localStride, values)
        else
           nf90_put_var_2D_EightByteInt = &
                nf_put_vara_int64(ncid, varid, localStart, localCount, values)
        end if
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_2D_EightByteInt = &
                nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
        else if(present(stride)) then
           nf90_put_var_2D_EightByteInt = &
                nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
        else
           nf90_put_var_2D_EightByteInt = &
                nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
        end if
     endif
  endif
end function nf90_put_var_2D_EightByteInt
function nf90_put_var_3D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_3D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_put_var_3D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_put_var_3D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_3D_EightByteInt = &
                nf_put_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, values)
        else if(present(stride)) then
           nf90_put_var_3D_EightByteInt = &
                nf_put_vars_int64(ncid, varid, localStart, localCount, localStride, values)
        else
           nf90_put_var_3D_EightByteInt = &
                nf_put_vara_int64(ncid, varid, localStart, localCount, values)
        end if
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_3D_EightByteInt = &
                nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
        else if(present(stride)) then
           nf90_put_var_3D_EightByteInt = &
                nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
        else
           nf90_put_var_3D_EightByteInt = &
                nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
        end if
     endif
  endif
end function nf90_put_var_3D_EightByteInt
function nf90_put_var_4D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_4D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_put_var_4D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_put_var_4D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_4D_EightByteInt = &
                nf_put_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, values)
        else if(present(stride)) then
           nf90_put_var_4D_EightByteInt = &
                nf_put_vars_int64(ncid, varid, localStart, localCount, localStride, values)
        else
           nf90_put_var_4D_EightByteInt = &
                nf_put_vara_int64(ncid, varid, localStart, localCount, values)
        end if
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_4D_EightByteInt = &
                nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
        else if(present(stride)) then
           nf90_put_var_4D_EightByteInt = &
                nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
        else
           nf90_put_var_4D_EightByteInt = &
                nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
        end if
     end if
  end if
end function nf90_put_var_4D_EightByteInt
function nf90_put_var_5D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_5D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_put_var_5D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_put_var_5D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_5D_EightByteInt = &
                nf_put_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, values)
        else if(present(stride)) then
           nf90_put_var_5D_EightByteInt = &
                nf_put_vars_int64(ncid, varid, localStart, localCount, localStride, values)
        else
           nf90_put_var_5D_EightByteInt = &
                nf_put_vara_int64(ncid, varid, localStart, localCount, values)
        end if
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_5D_EightByteInt = &
                nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
        else if(present(stride)) then
           nf90_put_var_5D_EightByteInt = &
                nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
        else
           nf90_put_var_5D_EightByteInt = &
                nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
        end if
     end if
  end if
end function nf90_put_var_5D_EightByteInt
function nf90_put_var_6D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_6D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_put_var_6D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_put_var_6D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_6D_EightByteInt = &
                nf_put_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, values)
        else if(present(stride)) then
           nf90_put_var_6D_EightByteInt = &
                nf_put_vars_int64(ncid, varid, localStart, localCount, localStride, values)
        else
           nf90_put_var_6D_EightByteInt = &
                nf_put_vara_int64(ncid, varid, localStart, localCount, values)
        end if
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_6D_EightByteInt = &
                nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
        else if(present(stride)) then
           nf90_put_var_6D_EightByteInt = &
                nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
        else
           nf90_put_var_6D_EightByteInt = &
                nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
        end if
     end if
  end if
end function nf90_put_var_6D_EightByteInt
function nf90_put_var_7D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :, :, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_7D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_put_var_7D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_put_var_7D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_7D_EightByteInt = &
                nf_put_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, values)
        else if(present(stride)) then
           nf90_put_var_7D_EightByteInt = &
                nf_put_vars_int64(ncid, varid, localStart, localCount, localStride, values)
        else
           nf90_put_var_7D_EightByteInt = &
                nf_put_vara_int64(ncid, varid, localStart, localCount, values)
        end if
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_put_var_7D_EightByteInt = &
                nf_put_varm_int(ncid, varid, localStart, localCount, localStride, localMap, int(values))
        else if(present(stride)) then
           nf90_put_var_7D_EightByteInt = &
                nf_put_vars_int(ncid, varid, localStart, localCount, localStride, int(values))
        else
           nf90_put_var_7D_EightByteInt = &
                nf_put_vara_int(ncid, varid, localStart, localCount, int(values))
        end if
     end if
  end if
end function nf90_put_var_7D_EightByteInt
function nf90_get_var_1D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_1D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  integer, dimension(size(values)) :: defaultIntArray
  integer (kind = selected_int_kind(18)), dimension(size(values)) :: defaultInt8Array
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_get_var_1D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_get_var_1D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_1D_EightByteInt = &
                nf_get_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, defaultInt8Array)
        else if(present(stride)) then
           nf90_get_var_1D_EightByteInt = &
                nf_get_vars_int64(ncid, varid, localStart, localCount, localStride, defaultInt8Array)
        else
           nf90_get_var_1D_EightByteInt = &
                nf_get_vara_int64(ncid, varid, localStart, localCount, defaultInt8Array)
        end if
        values(:) = reshape(defaultInt8Array(:), shape(values))
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_1D_EightByteInt = &
                nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
        else if(present(stride)) then
           nf90_get_var_1D_EightByteInt = &
                nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
        else
           nf90_get_var_1D_EightByteInt = &
                nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
        end if
        values(:) = reshape(defaultIntArray(:), shape(values))
     endif
  endif
end function nf90_get_var_1D_EightByteInt
function nf90_get_var_2D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_2D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  integer, dimension(size(values)) :: defaultIntArray
  integer (kind = selected_int_kind(18)), dimension(size(values)) :: defaultInt8Array
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_get_var_2D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_get_var_2D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_2D_EightByteInt = &
                nf_get_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, defaultInt8Array)
        else if(present(stride)) then
           nf90_get_var_2D_EightByteInt = &
                nf_get_vars_int64(ncid, varid, localStart, localCount, localStride, defaultInt8Array)
        else
           nf90_get_var_2D_EightByteInt = &
                nf_get_vara_int64(ncid, varid, localStart, localCount, defaultInt8Array)
        end if
        values(:, :) = reshape(defaultInt8Array(:), shape(values))
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_2D_EightByteInt = &
                nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
        else if(present(stride)) then
           nf90_get_var_2D_EightByteInt = &
                nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
        else
           nf90_get_var_2D_EightByteInt = &
                nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
        end if
        values(:, :) = reshape(defaultIntArray(:), shape(values))
     end if
  end if
end function nf90_get_var_2D_EightByteInt
function nf90_get_var_3D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_3D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  integer, dimension(size(values)) :: defaultIntArray
  integer (kind = selected_int_kind(18)), dimension(size(values)) :: defaultInt8Array
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_get_var_3D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_get_var_3D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_3D_EightByteInt = &
                nf_get_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, defaultInt8Array)
        else if(present(stride)) then
           nf90_get_var_3D_EightByteInt = &
                nf_get_vars_int64(ncid, varid, localStart, localCount, localStride, defaultInt8Array)
        else
           nf90_get_var_3D_EightByteInt = &
                nf_get_vara_int64(ncid, varid, localStart, localCount, defaultInt8Array)
        end if
        values(:, :, :) = reshape(defaultInt8Array(:), shape(values))
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_3D_EightByteInt = &
                nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
        else if(present(stride)) then
           nf90_get_var_3D_EightByteInt = &
                nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
        else
           nf90_get_var_3D_EightByteInt = &
                nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
        end if
        values(:, :, :) = reshape(defaultIntArray(:), shape(values))
     end if
  end if
end function nf90_get_var_3D_EightByteInt
function nf90_get_var_4D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_4D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  integer, dimension(size(values)) :: defaultIntArray
  integer (kind = selected_int_kind(18)), dimension(size(values)) :: defaultInt8Array
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_get_var_4D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_get_var_4D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_4D_EightByteInt = &
                nf_get_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, defaultInt8Array)
        else if(present(stride)) then
           nf90_get_var_4D_EightByteInt = &
                nf_get_vars_int64(ncid, varid, localStart, localCount, localStride, defaultInt8Array)
        else
           nf90_get_var_4D_EightByteInt = &
                nf_get_vara_int64(ncid, varid, localStart, localCount, defaultInt8Array)
        end if
        values(:, :, :, :) = reshape(defaultInt8Array(:), shape(values))
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_4D_EightByteInt = &
                nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
        else if(present(stride)) then
           nf90_get_var_4D_EightByteInt = &
                nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
        else
           nf90_get_var_4D_EightByteInt = &
                nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
        end if
        values(:, :, :, :) = reshape(defaultIntArray(:), shape(values))
     end if
  end if
end function nf90_get_var_4D_EightByteInt
function nf90_get_var_5D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_5D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  integer, dimension(size(values)) :: defaultIntArray
  integer (kind = selected_int_kind(18)), dimension(size(values)) :: defaultInt8Array
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_get_var_5D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_get_var_5D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_5D_EightByteInt = &
                nf_get_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, defaultInt8Array)
        else if(present(stride)) then
           nf90_get_var_5D_EightByteInt = &
                nf_get_vars_int64(ncid, varid, localStart, localCount, localStride, defaultInt8Array)
        else
           nf90_get_var_5D_EightByteInt = &
                nf_get_vara_int64(ncid, varid, localStart, localCount, defaultInt8Array)
        end if
        values(:, :, :, :, :) = reshape(defaultInt8Array(:), shape(values))
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_5D_EightByteInt = &
                nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
        else if(present(stride)) then
           nf90_get_var_5D_EightByteInt = &
                nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
        else
           nf90_get_var_5D_EightByteInt = &
                nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
        end if
        values(:, :, :, :, :) = reshape(defaultIntArray(:), shape(values))
     end if
  end if
end function nf90_get_var_5D_EightByteInt
function nf90_get_var_6D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_6D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  integer, dimension(size(values)) :: defaultIntArray
  integer (kind = selected_int_kind(18)), dimension(size(values)) :: defaultInt8Array
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_get_var_6D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_get_var_6D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_6D_EightByteInt = &
                nf_get_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, defaultInt8Array)
        else if(present(stride)) then
           nf90_get_var_6D_EightByteInt = &
                nf_get_vars_int64(ncid, varid, localStart, localCount, localStride, defaultInt8Array)
        else
           nf90_get_var_6D_EightByteInt = &
                nf_get_vara_int64(ncid, varid, localStart, localCount, defaultInt8Array)
        end if
        values(:, :, :, :, :, :) = reshape(defaultInt8Array(:), shape(values))
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_6D_EightByteInt = &
                nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
        else if(present(stride)) then
           nf90_get_var_6D_EightByteInt = &
                nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
        else
           nf90_get_var_6D_EightByteInt = &
                nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
        end if
        values(:, :, :, :, :, :) = reshape(defaultIntArray(:), shape(values))
     end if
  end if
end function nf90_get_var_6D_EightByteInt
function nf90_get_var_7D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :, :, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_7D_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride, localMap
  integer :: numDims, counter, format_num
  integer, dimension(size(values)) :: defaultIntArray
  integer (kind = selected_int_kind(18)), dimension(size(values)) :: defaultInt8Array
  ! Set local arguments to default values
  numDims = size(shape(values))
  localStart (: ) = 1
  localCount (:numDims ) = shape(values)
  localCount (numDims+1:) = 1
  localStride(: ) = 1
  localMap (:numDims ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  if(present(start)) localStart (:size(start) ) = start(:)
  if(present(count)) localCount (:size(count) ) = count(:)
  if(present(stride)) localStride(:size(stride)) = stride(:)
  nf90_get_var_7D_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_get_var_7D_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_7D_EightByteInt = &
                nf_get_varm_int64(ncid, varid, localStart, localCount, localStride, localMap, defaultInt8Array)
        else if(present(stride)) then
           nf90_get_var_7D_EightByteInt = &
                nf_get_vars_int64(ncid, varid, localStart, localCount, localStride, defaultInt8Array)
        else
           nf90_get_var_7D_EightByteInt = &
                nf_get_vara_int64(ncid, varid, localStart, localCount, defaultInt8Array)
        end if
        values(:, :, :, :, :, :, :) = reshape(defaultInt8Array(:), shape(values))
     else
        if(present(map)) then
           localMap (:size(map)) = map(:)
           nf90_get_var_7D_EightByteInt = &
                nf_get_varm_int(ncid, varid, localStart, localCount, localStride, localMap, defaultIntArray)
        else if(present(stride)) then
           nf90_get_var_7D_EightByteInt = &
                nf_get_vars_int(ncid, varid, localStart, localCount, localStride, defaultIntArray)
        else
           nf90_get_var_7D_EightByteInt = &
                nf_get_vara_int(ncid, varid, localStart, localCount, defaultIntArray)
        end if
        values(:, :, :, :, :, :, :) = reshape(defaultIntArray(:), shape(values))
     end if
  end if
end function nf90_get_var_7D_EightByteInt
function nf90_put_var_EightByteInt(ncid, varid, values, start)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start
  integer :: nf90_put_var_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localIndex
  integer :: format_num
  ! Set local arguments to default values
  localIndex(:) = 1
  if(present(start)) localIndex(:size(start)) = start(:)
  nf90_put_var_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_put_var_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        nf90_put_var_EightByteInt = nf_put_var1_int64(ncid, varid, localIndex, values)
     else
        nf90_put_var_EightByteInt = nf_put_var1_int(ncid, varid, localIndex, int(values))
     endif
  endif
end function nf90_put_var_EightByteInt
function nf90_get_var_EightByteInt(ncid, varid, values, start)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start
  integer :: nf90_get_var_EightByteInt
  integer, dimension(nf90_max_var_dims) :: localIndex
  integer :: format_num
  integer :: defaultInteger
  integer (kind = selected_int_kind(18)) :: defaultInteger8
  ! Set local arguments to default values
  localIndex(:) = 1
  if(present(start)) localIndex(:size(start)) = start(:)
  nf90_get_var_EightByteInt = nf_inq_format(ncid, format_num)
  if (nf90_get_var_EightByteInt .eq. nf90_noerr) then
     if (format_num .eq. nf90_format_netcdf4 .OR. &
         format_num .eq. nf90_format_cdf5) then
        nf90_get_var_EightByteInt = nf_get_var1_int64(ncid, varid, localIndex, defaultInteger8)
        values = defaultInteger8
     else
        nf90_get_var_EightByteInt = nf_get_var1_int(ncid, varid, localIndex, defaultInteger)
        values = defaultInteger
     endif
  endif
end function nf90_get_var_EightByteInt
  !
  ! NetCDF-4 extra routines:
  !
  ! -----------
  function nf90_create_par(path, cmode, comm, info, ncid, cache_size, &
       cache_nelems, cache_preemption)
    character (len = *), intent(in) :: path
    integer, intent(in) :: cmode
    integer, intent(in) :: comm
    integer, intent(in) :: info
    integer, intent(out) :: ncid
    integer, optional, intent(in) :: cache_size, cache_nelems
    real, optional, intent(in) :: cache_preemption
    integer :: size_in, nelems_in, preemption_in
    integer :: size_out, nelems_out, preemption_out, ret
    integer :: nf90_create_par
    ! If the user specified chuck cache parameters, use them. But user
    ! may have specified one, two, or three settings. Leave the others
    ! unchanged.
    if (present(cache_size) .or. present(cache_nelems) .or. &
         present(cache_preemption)) then
       ret = nf_get_chunk_cache(size_in, nelems_in, preemption_in)
       if (ret .ne. nf90_noerr) then
          nf90_create_par = ret
          return
       end if
       if (present(cache_size)) then
          size_out = cache_size
       else
          size_out = size_in
       end if
       if (present(cache_nelems)) then
          nelems_out = cache_nelems
       else
          nelems_out = nelems_in
       end if
       if (present(cache_preemption)) then
          preemption_out = int(cache_preemption * 100)
       else
          preemption_out = preemption_in
       end if
       nf90_create_par = nf_set_chunk_cache(size_out, nelems_out, preemption_out)
       if (nf90_create_par .ne. nf90_noerr) return
    end if
    nf90_create_par = nf_create_par(path, cmode, comm, info, ncid)
  end function nf90_create_par
  ! -----------
  function nf90_open_par(path, cmode, comm, info, ncid, cache_size, &
       cache_nelems, cache_preemption)
    character (len = *), intent(in) :: path
    integer, intent(in) :: cmode
    integer, intent(in) :: comm
    integer, intent(in) :: info
    integer, intent(out) :: ncid
    integer, optional, intent(in) :: cache_size, cache_nelems
    real, optional, intent(in) :: cache_preemption
    integer :: size_in, nelems_in, preemption_in
    integer :: size_out, nelems_out, preemption_out, ret
    integer :: nf90_open_par
    ! If the user specified chuck cache parameters, use them. But user
    ! may have specified one, two, or three settings. Leave the others
    ! unchanged.
    if (present(cache_size) .or. present(cache_nelems) .or. &
         present(cache_preemption)) then
       ret = nf_get_chunk_cache(size_in, nelems_in, preemption_in)
       if (ret .ne. nf90_noerr) then
          nf90_open_par = ret
          return
       end if
       if (present(cache_size)) then
          size_out = cache_size
       else
          size_out = size_in
       end if
       if (present(cache_nelems)) then
          nelems_out = cache_nelems
       else
          nelems_out = nelems_in
       end if
       if (present(cache_preemption)) then
          preemption_out = int(cache_preemption * 100)
       else
          preemption_out = preemption_in
       end if
       nf90_open_par = nf_set_chunk_cache(size_out, nelems_out, preemption_out)
       if (nf90_open_par .ne. nf90_noerr) return
    end if
    nf90_open_par = nf_open_par(path, cmode, comm, info, ncid)
  end function nf90_open_par
  ! -----------
  function nf90_var_par_access(ncid, varid, access)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: access
    integer :: nf90_var_par_access
    nf90_var_par_access = nf_var_par_access(ncid, varid, access)
  end function nf90_var_par_access
  ! -----------
  function nf90_inq_ncid(ncid, name, grp_ncid)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: name
    integer, intent(out) :: grp_ncid
    integer :: nf90_inq_ncid
    nf90_inq_ncid = nf_inq_ncid(ncid, name, grp_ncid)
  end function nf90_inq_ncid
  ! -----------
  function nf90_inq_grps(ncid, numgrps, ncids)
    integer, intent(in) :: ncid
    integer, intent(out) :: numgrps
    integer, dimension(:), intent(out) :: ncids
    integer :: nf90_inq_grps
    nf90_inq_grps = nf_inq_grps(ncid, numgrps, ncids)
  end function nf90_inq_grps
  ! -----------
  function nf90_inq_grpname_len(ncid, len)
    integer, intent(in) :: ncid
    integer, intent(out) :: len
    integer :: nf90_inq_grpname_len
    nf90_inq_grpname_len = nf_inq_grpname_len(ncid, len)
  end function nf90_inq_grpname_len
  ! -----------
  function nf90_inq_grp_ncid(ncid, name, grpid)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: name
    integer, intent(out) :: grpid
    integer :: nf90_inq_grp_ncid
    nf90_inq_grp_ncid = nf_inq_grp_ncid(ncid, name, grpid)
  end function nf90_inq_grp_ncid
  ! -----------
  function nf90_inq_grp_full_ncid(ncid, full_name, grpid)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: full_name
    integer, intent(out) :: grpid
    integer :: nf90_inq_grp_full_ncid
    nf90_inq_grp_full_ncid = nf_inq_grp_full_ncid(ncid, full_name, grpid)
  end function nf90_inq_grp_full_ncid
  ! -----------
  function nf90_inq_grp_parent(ncid, parent_ncid)
    integer, intent(in) :: ncid
    integer, intent(out) :: parent_ncid
    integer :: nf90_inq_grp_parent
    nf90_inq_grp_parent = nf_inq_grp_parent(ncid, parent_ncid)
  end function nf90_inq_grp_parent
  ! -----------
  function nf90_inq_grpname(ncid, name)
    integer, intent(in) :: ncid
    character (len = *), intent(out) :: name
    integer :: nf90_inq_grpname
    nf90_inq_grpname = nf_inq_grpname(ncid, name)
  end function nf90_inq_grpname
  ! -----------
  function nf90_inq_grpname_full(ncid, len, name)
    integer, intent(in) :: ncid
    integer, intent(out) :: len
    character (len = *), intent(out) :: name
    integer :: nf90_inq_grpname_full
    nf90_inq_grpname_full = nf_inq_grpname_full(ncid, len, name)
  end function nf90_inq_grpname_full
  ! -----------
  function nf90_inq_varids(ncid, nvars, varids)
    integer, intent(in) :: ncid
    integer, intent(out) :: nvars
    integer, dimension(:), intent(out) :: varids
    integer :: nf90_inq_varids
    nf90_inq_varids = nf_inq_varids(ncid, nvars, varids)
  end function nf90_inq_varids
  ! -----------
  function nf90_inq_dimids(ncid, ndims, dimids, include_parents)
    integer, intent(in) :: ncid
    integer, intent(out) :: ndims
    integer, dimension(:), intent(out) :: dimids
    integer, intent(out) :: include_parents
    integer :: nf90_inq_dimids
    nf90_inq_dimids = nf_inq_dimids(ncid, ndims, dimids, include_parents)
  end function nf90_inq_dimids
  ! -----------
  function nf90_inq_typeids(ncid, ntypes, typeids)
    integer, intent(in) :: ncid
    integer, optional, intent(out) :: ntypes
    integer, dimension(:), optional, intent(out) :: typeids
    integer :: nf90_inq_typeids
    nf90_inq_typeids = nf_inq_typeids(ncid, ntypes, typeids)
  end function nf90_inq_typeids
  ! -----------
  function nf90_inq_typeid(ncid, name, typeid)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: name
    integer, optional, intent(out) :: typeid
    integer :: nf90_inq_typeid
    nf90_inq_typeid = nf_inq_typeid(ncid, name, typeid)
  end function nf90_inq_typeid
  ! -----------
  function nf90_def_grp(parent_ncid, name, new_ncid)
    integer, intent(in) :: parent_ncid
    character (len = *), intent(in) :: name
    integer, intent(out) :: new_ncid
    integer :: nf90_def_grp
    nf90_def_grp = nf_def_grp(parent_ncid, name, new_ncid)
  end function nf90_def_grp
  ! -----------
  function nf90_rename_grp(grpid, name)
    integer, intent(in) :: grpid
    character (len = *), intent(in) :: name
    integer :: nf90_rename_grp
    nf90_rename_grp = nf_rename_grp(grpid, name)
  end function nf90_rename_grp
  ! -----------
  function nf90_def_compound(ncid, size, name, typeid)
    integer, intent(in) :: ncid
    integer, intent(in) :: size
    character (len = *), intent(in) :: name
    integer, intent(out) :: typeid
    integer :: nf90_def_compound
    nf90_def_compound = nf_def_compound(ncid, size, name, typeid)
  end function nf90_def_compound
  ! -----------
  function nf90_insert_compound(ncid, xtype, name, offset, field_typeid)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(in) :: name
    integer, intent(in) :: offset
    integer, intent(in) :: field_typeid
    integer :: nf90_insert_compound
    nf90_insert_compound = nf_insert_compound(ncid, xtype, name, offset, field_typeid)
  end function nf90_insert_compound
  ! -----------
  function nf90_insert_array_compound(ncid, xtype, name, offset, field_typeid, &
       ndims, dim_sizes)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(in) :: name
    integer, intent(in) :: offset
    integer, intent(in) :: field_typeid
    integer, intent(in) :: ndims
    integer, intent(in) :: dim_sizes
    integer :: nf90_insert_array_compound
    nf90_insert_array_compound = nf_insert_array_compound(ncid, xtype, name, &
         offset, field_typeid, ndims, dim_sizes)
  end function nf90_insert_array_compound
  ! -----------
  function nf90_inq_type(ncid, xtype, name, size)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: size
    integer :: nf90_inq_type
    nf90_inq_type = nf_inq_type(ncid, xtype, name, size)
  end function nf90_inq_type
  ! -----------
  function nf90_inq_compound(ncid, xtype, name, size, nfields)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: size
    integer, intent(out) :: nfields
    integer :: nf90_inq_compound
    nf90_inq_compound = nf_inq_compound(ncid, xtype, name, size, nfields)
  end function nf90_inq_compound
  ! -----------
  function nf90_inq_compound_name(ncid, xtype, name)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer :: nf90_inq_compound_name
    nf90_inq_compound_name = nf_inq_compound_name(ncid, xtype, name)
  end function nf90_inq_compound_name
  ! -----------
  function nf90_inq_compound_size(ncid, xtype, size)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(out) :: size
    integer :: nf90_inq_compound_size
    nf90_inq_compound_size = nf_inq_compound_size(ncid, xtype, size)
  end function nf90_inq_compound_size
  ! -----------
  function nf90_inq_compound_nfields(ncid, xtype, nfields)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(out) :: nfields
    integer :: nf90_inq_compound_nfields
    nf90_inq_compound_nfields = nf_inq_compound_nfields(ncid, xtype, nfields)
  end function nf90_inq_compound_nfields
  ! -----------
  function nf90_inq_compound_field(ncid, xtype, fieldid, name, offset, &
       field_typeid, ndims, dim_sizes)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    character (len = *), intent(out) :: name
    integer, intent(out) :: offset
    integer, intent(out) :: field_typeid
    integer, intent(out) :: ndims
    integer, intent(out) :: dim_sizes
    integer :: nf90_inq_compound_field
    nf90_inq_compound_field = nf_inq_compound_field(ncid, xtype, fieldid, name, offset, &
       field_typeid, ndims, dim_sizes)
  end function nf90_inq_compound_field
  ! -----------
  function nf90_inq_compound_fieldname(ncid, xtype, fieldid, name)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    character (len = *), intent(out) :: name
    integer :: nf90_inq_compound_fieldname
    nf90_inq_compound_fieldname = nf_inq_compound_fieldname(ncid, xtype, fieldid, name)
  end function nf90_inq_compound_fieldname
  ! -----------
  function nf90_inq_compound_fieldindex(ncid, xtype, name, fieldid)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(in) :: name
    integer, intent(out) :: fieldid
    integer :: nf90_inq_compound_fieldindex
    nf90_inq_compound_fieldindex = nf_inq_compound_fieldindex(ncid, xtype, name, fieldid)
  end function nf90_inq_compound_fieldindex
  ! -----------
  function nf90_inq_compound_fieldoffset(ncid, xtype, fieldid, offset)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    integer, intent(out) :: offset
    integer :: nf90_inq_compound_fieldoffset
    nf90_inq_compound_fieldoffset = nf_inq_compound_fieldoffset(ncid, xtype, fieldid, offset)
  end function nf90_inq_compound_fieldoffset
  ! -----------
  function nf90_inq_compound_fieldtype(ncid, xtype, fieldid, field_typeid)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    integer, intent(out) :: field_typeid
    integer :: nf90_inq_compound_fieldtype
    nf90_inq_compound_fieldtype = nf_inq_compound_fieldtype(ncid, xtype, fieldid, field_typeid)
  end function nf90_inq_compound_fieldtype
  ! -----------
  function nf90_inq_compound_fieldndims(ncid, xtype, fieldid, ndims)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    integer, intent(out) :: ndims
    integer :: nf90_inq_compound_fieldndims
    nf90_inq_compound_fieldndims = nf_inq_compound_fieldndims(ncid, xtype, fieldid, ndims)
  end function nf90_inq_compound_fieldndims
  ! -----------
  function nf90_inq_cmp_fielddim_sizes(ncid, xtype, fieldid, dim_sizes)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    integer, intent(out) :: dim_sizes
    integer :: nf90_inq_cmp_fielddim_sizes
    nf90_inq_cmp_fielddim_sizes = nf_inq_compound_fielddim_sizes(ncid, xtype, fieldid, dim_sizes)
  end function nf90_inq_cmp_fielddim_sizes
  ! -----------
  function nf90_def_vlen(ncid, name, base_typeid, xtypeid)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: name
    integer, intent(in) :: base_typeid
    integer, intent(out) :: xtypeid
    integer :: nf90_def_vlen
    nf90_def_vlen = nf_def_vlen(ncid, name, base_typeid, xtypeid)
  end function nf90_def_vlen
  ! -----------
  function nf90_inq_vlen(ncid, xtype, name, datum_size, base_nc_type)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: datum_size
    integer, intent(out) :: base_nc_type
    integer :: nf90_inq_vlen
    nf90_inq_vlen = nf_inq_vlen(ncid, xtype, name, datum_size, base_nc_type)
  end function nf90_inq_vlen
  ! -----------
  function nf90_free_vlen(vl)
    character (len = *), intent(in) :: vl
    integer :: nf90_free_vlen
    nf90_free_vlen = nf_free_vlen(vl)
  end function nf90_free_vlen
! ! -----------
  function nf90_def_enum(ncid, base_typeid, name, typeid)
    integer, intent(in) :: ncid
    integer, intent(in) :: base_typeid
    character (len = *), intent(in) :: name
    integer, intent(out) :: typeid
    integer :: nf90_def_enum
    nf90_def_enum = nf_def_enum(ncid, base_typeid, name, typeid)
  end function nf90_def_enum
! ! -----------
  function nf90_inq_user_type(ncid, xtype, name, size, base_typeid, nfields, class)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: size
    integer, intent(out) :: base_typeid
    integer, intent(out) :: nfields
    integer, intent(out) :: class
    integer :: nf90_inq_user_type
    nf90_inq_user_type = nf_inq_user_type(ncid, xtype, name, size, base_typeid, nfields, class)
  end function nf90_inq_user_type
  ! -----------
  function nf90_insert_enum(ncid, xtype, name, value)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(in) :: name
    integer, intent(in) :: value
    integer :: nf90_insert_enum
    nf90_insert_enum = nf_insert_enum(ncid, xtype, name, value)
  end function nf90_insert_enum
  ! -----------
  function nf90_inq_enum(ncid, xtype, name, base_nc_type, base_size, num_members)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: base_nc_type
    integer, intent(out) :: base_size
    integer, intent(out) :: num_members
    integer :: nf90_inq_enum
    nf90_inq_enum = nf_inq_enum(ncid, xtype, name, base_nc_type, base_size, num_members)
  end function nf90_inq_enum
  ! -----------
  function nf90_inq_enum_member(ncid, xtype, idx, name, value)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: idx
    character (len = *), intent(out) :: name
    integer, intent(in) :: value
    integer :: nf90_inq_enum_member
    nf90_inq_enum_member = nf_inq_enum_member(ncid, xtype, idx, name, value)
  end function nf90_inq_enum_member
  ! -----------
  function nf90_inq_enum_ident(ncid, xtype, value, idx)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: value
    integer, intent(out) :: idx
    integer :: nf90_inq_enum_ident
    nf90_inq_enum_ident = nf_inq_enum_ident(ncid, xtype, value, idx)
  end function nf90_inq_enum_ident
  ! -----------
  function nf90_def_opaque(ncid, size, name, xtype)
    integer, intent(in) :: ncid
    integer, intent(in) :: size
    character (len = *), intent(in) :: name
    integer, intent(out) :: xtype
    integer :: nf90_def_opaque
    nf90_def_opaque = nf_def_opaque(ncid, size, name, xtype)
  end function nf90_def_opaque
  ! -----------
  function nf90_inq_opaque(ncid, xtype, name, size)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: size
    integer :: nf90_inq_opaque
    nf90_inq_opaque = nf_inq_opaque(ncid, xtype, name, size)
  end function nf90_inq_opaque
  ! -----------
  function nf90_def_var_chunking(ncid, varid, contiguous, chunksizes)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: contiguous
    integer, dimension(:), intent(in) :: chunksizes
    integer :: nf90_def_var_chunking
    nf90_def_var_chunking = nf_def_var_chunking(ncid, varid, contiguous, chunksizes)
  end function nf90_def_var_chunking
  ! -----------
  function nf90_def_var_deflate(ncid, varid, shuffle, deflate, deflate_level)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: shuffle
    integer, intent(in) :: deflate
    integer, intent(in) :: deflate_level
    integer :: nf90_def_var_deflate
    nf90_def_var_deflate = nf_def_var_deflate(ncid, varid, shuffle, deflate, deflate_level)
  end function nf90_def_var_deflate
  ! -----------
  function nf90_def_var_szip(ncid, varid, options_mask, pixels_per_block)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: options_mask
    integer, intent(in) :: pixels_per_block
    integer :: nf90_def_var_szip
    nf90_def_var_szip = nf_def_var_szip(ncid, varid, options_mask, pixels_per_block)
  end function nf90_def_var_szip
  ! -----------
  function nf90_def_var_fletcher32(ncid, varid, fletcher32)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: fletcher32
    integer :: nf90_def_var_fletcher32
    nf90_def_var_fletcher32 = nf_def_var_fletcher32(ncid, varid, fletcher32)
  end function nf90_def_var_fletcher32
  ! -----------
  function nf90_inq_var_chunking(ncid, varid, contiguous, chunksizes)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: contiguous
    integer, dimension(:), intent(out) :: chunksizes
    integer :: nf90_inq_var_chunking
    nf90_inq_var_chunking = nf_inq_var_chunking(ncid, varid, contiguous, chunksizes)
  end function nf90_inq_var_chunking
  ! -----------
  function nf90_inq_var_deflate(ncid, varid, shuffle, deflate, deflate_level)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: shuffle
    integer, intent(out) :: deflate
    integer, intent(out) :: deflate_level
    integer :: nf90_inq_var_deflate
    nf90_inq_var_deflate = nf_inq_var_deflate(ncid, varid, shuffle, deflate, deflate_level)
  end function nf90_inq_var_deflate
  ! -----------
  function nf90_inq_var_szip(ncid, varid, options_mask, pixels_per_block)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: options_mask
    integer, intent(out) :: pixels_per_block
    integer :: nf90_inq_var_szip
    nf90_inq_var_szip = nf_inq_var_szip(ncid, varid, options_mask, pixels_per_block)
  end function nf90_inq_var_szip
  ! -----------
  function nf90_inq_var_fletcher32(ncid, varid, fletcher32)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: fletcher32
    integer :: nf90_inq_var_fletcher32
    nf90_inq_var_fletcher32 = nf_inq_var_fletcher32(ncid, varid, fletcher32)
  end function nf90_inq_var_fletcher32
  ! -----------
  function nf90_def_var_endian(ncid, varid, endian)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: endian
    integer :: nf90_def_var_endian
    nf90_def_var_endian = nf_def_var_endian(ncid, varid, endian)
  end function nf90_def_var_endian
  ! -----------
  function nf90_inq_var_endian(ncid, varid, endian)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: endian
    integer :: nf90_inq_var_endian
    nf90_inq_var_endian = nf_inq_var_endian(ncid, varid, endian)
  end function nf90_inq_var_endian
  ! -----------
  function nf90_def_var_filter(ncid, varid, filterid, nparams, params)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: filterid
    integer, intent(in) :: nparams
    integer, intent(in) :: params(*)
    integer :: nf90_def_var_filter
    nf90_def_var_filter = nf_def_var_filter(ncid, varid, filterid, nparams, params)
  end function nf90_def_var_filter
  ! -----------
  function nf90_inq_var_filter(ncid, varid, filterid, nparams, params)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: filterid
    integer, intent(out) :: nparams
    integer, dimension(:), intent(out) :: params
    integer :: nf90_inq_var_filter
    nf90_inq_var_filter = nf_inq_var_filter(ncid, varid, filterid, nparams, params)
  end function nf90_inq_var_filter
  ! -----------
! function nf90_def_var_fill(ncid, varid, no_fill, fill)
! integer, intent(in) :: ncid
! integer, intent(in) :: varid
! integer, intent(in) :: no_fill
! integer, intent(in) :: fill
! integer :: nf90_def_var_fill
! nf90_def_var_fill = nf_def_var_fill(ncid, varid, no_fill, fill)
! end function nf90_def_var_fill
  ! -----------
! function nf90_inq_var_fill(ncid, varid, no_fill, fill)
! integer, intent(in) :: ncid
! integer, intent(in) :: varid
! integer, intent(out) :: no_fill
! integer, intent(out) :: fill
! integer :: nf90_inq_var_fill
! nf90_inq_var_fill = nf_inq_var_fill(ncid, varid, no_fill, fill)
! end function nf90_inq_var_fill
  function nf90_def_var_fill_OneByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    integer(kind=selected_int_kind(2)), intent(in) :: fill
    integer :: nf90_def_var_fill_OneByteInt
    nf90_def_var_fill_OneByteInt = nf_def_var_fill(ncid, varid, no_fill, fill)
  end function nf90_def_var_fill_OneByteInt
  ! -----------
  function nf90_def_var_fill_TwoByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    integer(kind=selected_int_kind(4)), intent(in) :: fill
    integer :: nf90_def_var_fill_TwoByteInt
    nf90_def_var_fill_TwoByteInt = nf_def_var_fill(ncid, varid, no_fill, fill)
  end function nf90_def_var_fill_TwoByteInt
  ! -----------
  function nf90_def_var_fill_FourByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    integer(kind=selected_int_kind(9)), intent(in) :: fill
    integer :: nf90_def_var_fill_FourByteInt
    nf90_def_var_fill_FourByteInt = nf_def_var_fill(ncid, varid, no_fill, fill)
  end function nf90_def_var_fill_FourByteInt
  ! -----------
  function nf90_def_var_fill_EightByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    integer(kind=selected_int_kind(18)), intent(in) :: fill
    integer :: nf90_def_var_fill_EightByteInt
    nf90_def_var_fill_EightByteInt = nf_def_var_fill(ncid, varid, no_fill, fill)
  end function nf90_def_var_fill_EightByteInt
  ! -----------
  function nf90_def_var_fill_FourByteReal(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    real(kind=selected_real_kind(P =  6, R =  37)), intent(in) :: fill
    integer :: nf90_def_var_fill_FourByteReal
    nf90_def_var_fill_FourByteReal = nf_def_var_fill(ncid, varid, no_fill, fill)
  end function nf90_def_var_fill_FourByteReal
  ! -----------
  function nf90_def_var_fill_EightByteReal(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    real(kind=selected_real_kind(P =  13, R =  307)), intent(in) :: fill
    integer :: nf90_def_var_fill_EightByteReal
    nf90_def_var_fill_EightByteReal = nf_def_var_fill(ncid, varid, no_fill, fill)
  end function nf90_def_var_fill_EightByteReal
  ! -----------
  function nf90_inq_var_fill_OneByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    integer(kind=selected_int_kind(2)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_OneByteInt
    nf90_inq_var_fill_OneByteInt = nf_inq_var_fill(ncid, varid, no_fill, fill)
  end function nf90_inq_var_fill_OneByteInt
  ! -----------
  function nf90_inq_var_fill_TwoByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    integer(kind=selected_int_kind(4)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_TwoByteInt
    nf90_inq_var_fill_TwoByteInt = nf_inq_var_fill(ncid, varid, no_fill, fill)
  end function nf90_inq_var_fill_TwoByteInt
  ! -----------
  function nf90_inq_var_fill_FourByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    integer(kind=selected_int_kind(9)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_FourByteInt
    nf90_inq_var_fill_FourByteInt = nf_inq_var_fill(ncid, varid, no_fill, fill)
  end function nf90_inq_var_fill_FourByteInt
  ! -----------
  function nf90_inq_var_fill_EightByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    integer(kind=selected_int_kind(18)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_EightByteInt
    nf90_inq_var_fill_EightByteInt = nf_inq_var_fill(ncid, varid, no_fill, fill)
  end function nf90_inq_var_fill_EightByteInt
  ! -----------
  function nf90_inq_var_fill_FourByteReal(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    real(kind=selected_real_kind(P =  6, R =  37)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_FourByteReal
    nf90_inq_var_fill_FourByteReal = nf_inq_var_fill(ncid, varid, no_fill, fill)
  end function nf90_inq_var_fill_FourByteReal
  ! -----------
  function nf90_inq_var_fill_EightByteReal(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    real(kind=selected_real_kind(P =  13, R =  307)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_EightByteReal
    nf90_inq_var_fill_EightByteReal = nf_inq_var_fill(ncid, varid, no_fill, fill)
  end function nf90_inq_var_fill_EightByteReal
  ! -----------
  function nf90_put_att_any(ncid, varid, name, typeid, length, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer, intent( in) :: typeid, length
    character(len = *), intent( in) :: values
    integer :: nf90_put_att_any
    nf90_put_att_any = nf_put_att(ncid, varid, name, typeid, length, values)
  end function nf90_put_att_any
  ! -----------
  function nf90_get_att_any(ncid, varid, name, length, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer, intent( in) :: length
    character(len = *), intent( in) :: values
    integer :: nf90_get_att_any
    nf90_get_att_any = nf_get_att(ncid, varid, name, values)
  end function nf90_get_att_any
  ! -----------
   function nf90_put_var_any(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_any
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride
     ! Set local arguments to default values
     localStart (:) = 1
     localCount (1) = len_trim(values); localCount (2:) = 1
     localStride(:) = 1
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     nf90_put_var_any = nf_put_vars(ncid, varid, localStart, localCount, localStride, values)
   end function nf90_put_var_any
  ! -----------
   function nf90_get_var_any(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_any
     integer, dimension(nf90_max_var_dims) :: localStart, localCount, localStride
     ! Set local arguments to default values
     localStart (:) = 1
     localCount (1) = len(values); localCount (2:) = 1
     localStride(:) = 1
     if(present(start)) localStart (:size(start) ) = start(:)
     if(present(count)) localCount (:size(count) ) = count(:)
     if(present(stride)) localStride(:size(stride)) = stride(:)
     nf90_get_var_any = nf_get_vars(ncid, varid, localStart, localCount, localStride, values)
   end function nf90_get_var_any
end module netcdf
