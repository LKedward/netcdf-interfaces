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
interface
module function nf90_inq_libvers()
  character(len = 80) :: nf90_inq_libvers
end function nf90_inq_libvers
module function nf90_strerror(ncerr)
  integer, intent( in) :: ncerr
  character(len = 80) :: nf90_strerror
end function nf90_strerror
module function nf90_inq_base_pe(ncid, pe)
  integer, intent( in) :: ncid
  integer, intent(out) :: pe
  integer :: nf90_inq_base_pe
end function nf90_inq_base_pe
module function nf90_set_base_pe(ncid, pe)
  integer, intent( in) :: ncid, pe
  integer :: nf90_set_base_pe
end function nf90_set_base_pe
module function nf90_create_mp(path, cmode, initalsz, basepe, chunksizehint, ncid)
  character (len = *), intent( in) :: path
  integer, intent( in) :: cmode, initalsz, basepe, chunksizehint
  integer, intent(out) :: ncid
  integer :: nf90_create_mp
end function nf90_create_mp
module function nf90_open_mp(path, mode, basepe, chunksizeint, ncid)
  character (len = *), intent( in) :: path
  integer, intent( in) :: mode, basepe, chunksizeint
  integer, intent(out) :: ncid
  integer :: nf90_open_mp
end function nf90_open_mp
module function nf90_set_fill(ncid, fillmode, old_mode)
  integer, intent( in) :: ncid, fillmode
  integer, intent(out) :: old_mode
  integer :: nf90_set_fill
end function nf90_set_fill
module function nf90_redef(ncid)
  integer, intent( in) :: ncid
  integer :: nf90_redef
end function nf90_redef
module function nf90_enddef(ncid, h_minfree, v_align, v_minfree, r_align)
  integer, intent( in) :: ncid
  integer, optional, intent( in) :: h_minfree, v_align, v_minfree, r_align
  integer :: nf90_enddef
end function nf90_enddef
module function nf90_sync(ncid)
  integer, intent( in) :: ncid
  integer :: nf90_sync
end function nf90_sync
module function nf90_abort(ncid)
  integer, intent( in) :: ncid
  integer :: nf90_abort
end function nf90_abort
module function nf90_close(ncid)
  integer, intent( in) :: ncid
  integer :: nf90_close
end function nf90_close
module function nf90_delete(name)
  character(len = *), intent( in) :: name
  integer :: nf90_delete
end function nf90_delete
module function nf90_inquire(ncid, nDimensions, nVariables, nAttributes, unlimitedDimId, formatNum)
  integer, intent( in) :: ncid
  integer, optional, intent(out) :: nDimensions, nVariables, nAttributes, unlimitedDimId, formatNum
  integer :: nf90_inquire
end function nf90_inquire
module function nf90_inq_path(ncid, pathlen, path)
  integer, intent(in) :: ncid
  integer, intent(inout) :: pathlen
  character(len = *), intent(inout) :: path
  integer :: nf90_inq_path
end function nf90_inq_path
module function nf90_inq_format(ncid, format_type)
  integer, intent(in) :: ncid
  integer, intent(out) :: format_type
  integer :: nf90_inq_format
end function nf90_inq_format
module function nf90_open(path, mode, ncid, chunksize, cache_size, cache_nelems, &
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
end function nf90_open
module function nf90_create(path, cmode, ncid, initialsize, chunksize, cache_size, &
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
end function nf90_create
module   function nf90_def_dim(ncid, name, len, dimid)
    integer, intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent( in) :: len
    integer, intent(out) :: dimid
    integer :: nf90_def_dim
  end function nf90_def_dim
module   function nf90_inq_dimid(ncid, name, dimid)
    integer, intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent(out) :: dimid
    integer :: nf90_inq_dimid
  end function nf90_inq_dimid
module   function nf90_rename_dim(ncid, dimid, name)
    integer, intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent( in) :: dimid
    integer :: nf90_rename_dim
  end function nf90_rename_dim
module   function nf90_inquire_dimension(ncid, dimid, name, len)
    integer, intent( in) :: ncid, dimid
    character (len = *), optional, intent(out) :: name
    integer, optional, intent(out) :: len
    integer :: nf90_inquire_dimension
  end function nf90_inquire_dimension
module   function nf90_copy_att(ncid_in, varid_in, name, ncid_out, varid_out)
    integer, intent( in) :: ncid_in, varid_in
    character (len = *), intent( in) :: name
    integer, intent( in) :: ncid_out, varid_out
    integer :: nf90_copy_att
  end function nf90_copy_att
module   function nf90_rename_att(ncid, varid, curname, newname)
    integer, intent( in) :: ncid, varid
    character (len = *), intent( in) :: curname, newname
    integer :: nf90_rename_att
  end function nf90_rename_att
module   function nf90_del_att(ncid, varid, name)
    integer, intent( in) :: ncid, varid
    character (len = *), intent( in) :: name
    integer :: nf90_del_att
  end function nf90_del_att
module   function nf90_inq_attname(ncid, varid, attnum, name)
    integer, intent( in) :: ncid, varid, attnum
    character (len = *), intent(out) :: name
    integer :: nf90_inq_attname
  end function nf90_inq_attname
module   function nf90_inquire_attribute(ncid, varid, name, xtype, len, attnum)
    integer, intent( in) :: ncid, varid
    character (len = *), intent( in) :: name
    integer, intent(out), optional :: xtype, len, attnum
    integer :: nf90_inquire_attribute
  end function nf90_inquire_attribute
module   function nf90_put_att_text(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    character(len = *), intent( in) :: values
    integer :: nf90_put_att_text
  end function nf90_put_att_text
module   function nf90_get_att_text(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    character(len = *), intent(out) :: values
    integer :: nf90_get_att_text
  end function nf90_get_att_text
module   function nf90_put_att_OneByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(2)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_OneByteInt
  end function nf90_put_att_OneByteInt
module   function nf90_put_att_one_OneByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(2)), intent( in) :: values
    integer :: nf90_put_att_one_OneByteInt
  end function nf90_put_att_one_OneByteInt
module   function nf90_get_att_OneByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(2)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_OneByteInt
  end function nf90_get_att_OneByteInt
module   function nf90_get_att_one_OneByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(2)), intent(out) :: values
    integer :: nf90_get_att_one_OneByteInt
  end function nf90_get_att_one_OneByteInt
module   function nf90_put_att_TwoByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(4)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_TwoByteInt
  end function nf90_put_att_TwoByteInt
module   function nf90_put_att_one_TwoByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(4)), intent( in) :: values
    integer :: nf90_put_att_one_TwoByteInt
  end function nf90_put_att_one_TwoByteInt
module   function nf90_get_att_TwoByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(4)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_TwoByteInt
  end function nf90_get_att_TwoByteInt
module   function nf90_get_att_one_TwoByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(4)), intent(out) :: values
    integer :: nf90_get_att_one_TwoByteInt
  end function nf90_get_att_one_TwoByteInt
module   function nf90_put_att_FourByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(9)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_FourByteInt
  end function nf90_put_att_FourByteInt
module   function nf90_put_att_one_FourByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(9)), intent( in) :: values
    integer :: nf90_put_att_one_FourByteInt
  end function nf90_put_att_one_FourByteInt
module   function nf90_get_att_FourByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(9)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_FourByteInt
  end function nf90_get_att_FourByteInt
module   function nf90_get_att_one_FourByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(9)), intent(out) :: values
    integer :: nf90_get_att_one_FourByteInt
  end function nf90_get_att_one_FourByteInt
module   function nf90_put_att_EightByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(18)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_EightByteInt
  end function nf90_put_att_EightByteInt
module   function nf90_put_att_one_EightByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(18)), intent( in) :: values
    integer :: nf90_put_att_one_EightByteInt
  end function nf90_put_att_one_EightByteInt
module   function nf90_get_att_EightByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(18)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_EightByteInt
  end function nf90_get_att_EightByteInt
module   function nf90_get_att_one_EightByteInt(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer (kind = selected_int_kind(18)), intent(out) :: values
    integer :: nf90_get_att_one_EightByteInt
  end function nf90_get_att_one_EightByteInt
module   function nf90_put_att_FourByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  6, R =  37)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_FourByteReal
  end function nf90_put_att_FourByteReal
module   function nf90_put_att_one_FourByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  6, R =  37)), intent( in) :: values
    integer :: nf90_put_att_one_FourByteReal
  end function nf90_put_att_one_FourByteReal
module   function nf90_get_att_FourByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  6, R =  37)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_FourByteReal
  end function nf90_get_att_FourByteReal
module   function nf90_get_att_one_FourByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  6, R =  37)), intent(out) :: values
    integer :: nf90_get_att_one_FourByteReal
  end function nf90_get_att_one_FourByteReal
module   function nf90_put_att_EightByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  13, R =  307)), dimension(:), intent( in) :: values
    integer :: nf90_put_att_EightByteReal
  end function nf90_put_att_EightByteReal
module   function nf90_put_att_one_EightByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  13, R =  307)), intent( in) :: values
    integer :: nf90_put_att_one_EightByteReal
  end function nf90_put_att_one_EightByteReal
module   function nf90_get_att_EightByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  13, R =  307)), dimension(:), intent(out) :: values
    integer :: nf90_get_att_EightByteReal
  end function nf90_get_att_EightByteReal
module   function nf90_get_att_one_EightByteReal(ncid, varid, name, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    real (kind = selected_real_kind(P =  13, R =  307)), intent(out) :: values
    integer :: nf90_get_att_one_EightByteReal
  end function nf90_get_att_one_EightByteReal
module   function nf90_def_var_Scalar(ncid, name, xtype, varid)
    integer, intent( in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent(in) :: xtype
    integer, intent(out) :: varid
    integer :: nf90_def_var_Scalar
  end function nf90_def_var_Scalar
module   function nf90_def_var_oneDim(ncid, name, xtype, dimids, varid, contiguous, &
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
  end function nf90_def_var_oneDim
module   function nf90_def_var_ManyDims(ncid, name, xtype, dimids, varid, contiguous, &
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
  end function nf90_def_var_ManyDims
module   function nf90_inq_varid(ncid, name, varid)
    integer, intent(in) :: ncid
    character (len = *), intent( in) :: name
    integer, intent(out) :: varid
    integer :: nf90_inq_varid
  end function nf90_inq_varid
module   function nf90_set_var_chunk_cache(ncid, varid, size, nelems, preemption)
    integer, intent(in) :: ncid, varid, size, nelems, preemption
    integer :: nf90_set_var_chunk_cache
  end function nf90_set_var_chunk_cache
module   function nf90_inquire_variable(ncid, varid, name, xtype, ndims, dimids, nAtts, &
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
  end function nf90_inquire_variable
module   function nf90_rename_var(ncid, varid, newname)
    integer, intent( in) :: ncid, varid
    character (len = *), intent( in) :: newname
    integer :: nf90_rename_var
  end function nf90_rename_var
module    function nf90_put_var_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_text
   end function nf90_put_var_text
module    function nf90_get_var_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_text
   end function nf90_get_var_text
module    function nf90_put_var_1D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_text
   end function nf90_put_var_1D_text
module    function nf90_put_var_2D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_text
   end function nf90_put_var_2D_text
module    function nf90_put_var_3D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_text
   end function nf90_put_var_3D_text
module    function nf90_put_var_4D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_text
   end function nf90_put_var_4D_text
module    function nf90_put_var_5D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_text
   end function nf90_put_var_5D_text
module    function nf90_put_var_6D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_text
   end function nf90_put_var_6D_text
module    function nf90_put_var_7D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_text
   end function nf90_put_var_7D_text
module    function nf90_get_var_1D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_text
   end function nf90_get_var_1D_text
module    function nf90_get_var_2D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_text
   end function nf90_get_var_2D_text
module    function nf90_get_var_3D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_text
   end function nf90_get_var_3D_text
module    function nf90_get_var_4D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_text
   end function nf90_get_var_4D_text
module    function nf90_get_var_5D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_text
   end function nf90_get_var_5D_text
module    function nf90_get_var_6D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_text
   end function nf90_get_var_6D_text
module    function nf90_get_var_7D_text(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_text
   end function nf90_get_var_7D_text
module    function nf90_put_var_OneByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_put_var_OneByteInt
   end function nf90_put_var_OneByteInt
module    function nf90_put_var_TwoByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_put_var_TwoByteInt
   end function nf90_put_var_TwoByteInt
module    function nf90_put_var_FourByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_put_var_FourByteInt
   end function nf90_put_var_FourByteInt
module    function nf90_put_var_FourByteReal(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_put_var_FourByteReal
   end function nf90_put_var_FourByteReal
module    function nf90_put_var_EightByteReal(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_put_var_EightByteReal
   end function nf90_put_var_EightByteReal
module    function nf90_get_var_OneByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_get_var_OneByteInt
   end function nf90_get_var_OneByteInt
module    function nf90_get_var_TwoByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_get_var_TwoByteInt
   end function nf90_get_var_TwoByteInt
module    function nf90_get_var_FourByteInt(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_get_var_FourByteInt
   end function nf90_get_var_FourByteInt
module    function nf90_get_var_FourByteReal(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_get_var_FourByteReal
   end function nf90_get_var_FourByteReal
module    function nf90_get_var_EightByteReal(ncid, varid, values, start)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start
     integer :: nf90_get_var_EightByteReal
   end function nf90_get_var_EightByteReal
module    function nf90_put_var_1D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_OneByteInt
   end function nf90_put_var_1D_OneByteInt
module    function nf90_put_var_2D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_OneByteInt
   end function nf90_put_var_2D_OneByteInt
module    function nf90_put_var_3D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_OneByteInt
   end function nf90_put_var_3D_OneByteInt
module    function nf90_put_var_4D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_OneByteInt
   end function nf90_put_var_4D_OneByteInt
module    function nf90_put_var_5D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_OneByteInt
   end function nf90_put_var_5D_OneByteInt
module    function nf90_put_var_6D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_OneByteInt
   end function nf90_put_var_6D_OneByteInt
module    function nf90_put_var_7D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_OneByteInt
   end function nf90_put_var_7D_OneByteInt
module    function nf90_put_var_1D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_TwoByteInt
   end function nf90_put_var_1D_TwoByteInt
module    function nf90_put_var_2D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_TwoByteInt
   end function nf90_put_var_2D_TwoByteInt
module    function nf90_put_var_3D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_TwoByteInt
   end function nf90_put_var_3D_TwoByteInt
module    function nf90_put_var_4D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_TwoByteInt
   end function nf90_put_var_4D_TwoByteInt
module    function nf90_put_var_5D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_TwoByteInt
   end function nf90_put_var_5D_TwoByteInt
module    function nf90_put_var_6D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_TwoByteInt
   end function nf90_put_var_6D_TwoByteInt
module    function nf90_put_var_7D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_TwoByteInt
   end function nf90_put_var_7D_TwoByteInt
module    function nf90_put_var_1D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_FourByteInt
   end function nf90_put_var_1D_FourByteInt
module    function nf90_put_var_2D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_FourByteInt
   end function nf90_put_var_2D_FourByteInt
module    function nf90_put_var_3D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_FourByteInt
   end function nf90_put_var_3D_FourByteInt
module    function nf90_put_var_4D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_FourByteInt
   end function nf90_put_var_4D_FourByteInt
module    function nf90_put_var_5D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_FourByteInt
   end function nf90_put_var_5D_FourByteInt
module    function nf90_put_var_6D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_FourByteInt
   end function nf90_put_var_6D_FourByteInt
module    function nf90_put_var_7D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_FourByteInt
   end function nf90_put_var_7D_FourByteInt
module     function nf90_put_var_1D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_FourByteReal
   end function nf90_put_var_1D_FourByteReal
module    function nf90_put_var_2D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_FourByteReal
   end function nf90_put_var_2D_FourByteReal
module    function nf90_put_var_3D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_FourByteReal
   end function nf90_put_var_3D_FourByteReal
module    function nf90_put_var_4D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_FourByteReal
   end function nf90_put_var_4D_FourByteReal
module    function nf90_put_var_5D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_FourByteReal
   end function nf90_put_var_5D_FourByteReal
module    function nf90_put_var_6D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_FourByteReal
   end function nf90_put_var_6D_FourByteReal
module    function nf90_put_var_7D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_FourByteReal
   end function nf90_put_var_7D_FourByteReal
module    function nf90_put_var_1D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_1D_EightByteReal
   end function nf90_put_var_1D_EightByteReal
module    function nf90_put_var_2D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_2D_EightByteReal
   end function nf90_put_var_2D_EightByteReal
module    function nf90_put_var_3D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_3D_EightByteReal
   end function nf90_put_var_3D_EightByteReal
module    function nf90_put_var_4D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_4D_EightByteReal
   end function nf90_put_var_4D_EightByteReal
module    function nf90_put_var_5D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_5D_EightByteReal
   end function nf90_put_var_5D_EightByteReal
module    function nf90_put_var_6D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_6D_EightByteReal
   end function nf90_put_var_6D_EightByteReal
module    function nf90_put_var_7D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :, :, :), &
                                      intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_7D_EightByteReal
   end function nf90_put_var_7D_EightByteReal
module    function nf90_get_var_1D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_OneByteInt
   end function nf90_get_var_1D_OneByteInt
module    function nf90_get_var_2D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_OneByteInt
   end function nf90_get_var_2D_OneByteInt
module    function nf90_get_var_3D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_OneByteInt
   end function nf90_get_var_3D_OneByteInt
module    function nf90_get_var_4D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_OneByteInt
   end function nf90_get_var_4D_OneByteInt
module    function nf90_get_var_5D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_OneByteInt
   end function nf90_get_var_5D_OneByteInt
module    function nf90_get_var_6D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_OneByteInt
   end function nf90_get_var_6D_OneByteInt
module    function nf90_get_var_7D_OneByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(2)), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_OneByteInt
   end function nf90_get_var_7D_OneByteInt
module    function nf90_get_var_1D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_TwoByteInt
   end function nf90_get_var_1D_TwoByteInt
module    function nf90_get_var_2D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_TwoByteInt
   end function nf90_get_var_2D_TwoByteInt
module    function nf90_get_var_3D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_TwoByteInt
   end function nf90_get_var_3D_TwoByteInt
module    function nf90_get_var_4D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_TwoByteInt
   end function nf90_get_var_4D_TwoByteInt
module    function nf90_get_var_5D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_TwoByteInt
   end function nf90_get_var_5D_TwoByteInt
module    function nf90_get_var_6D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_TwoByteInt
   end function nf90_get_var_6D_TwoByteInt
module    function nf90_get_var_7D_TwoByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(4)), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_TwoByteInt
   end function nf90_get_var_7D_TwoByteInt
module    function nf90_get_var_1D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_FourByteInt
   end function nf90_get_var_1D_FourByteInt
module    function nf90_get_var_2D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_FourByteInt
   end function nf90_get_var_2D_FourByteInt
module    function nf90_get_var_3D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_FourByteInt
   end function nf90_get_var_3D_FourByteInt
module    function nf90_get_var_4D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_FourByteInt
   end function nf90_get_var_4D_FourByteInt
module    function nf90_get_var_5D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_FourByteInt
   end function nf90_get_var_5D_FourByteInt
module    function nf90_get_var_6D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_FourByteInt
   end function nf90_get_var_6D_FourByteInt
module    function nf90_get_var_7D_FourByteInt(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     integer (kind = selected_int_kind(9)), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_FourByteInt
   end function nf90_get_var_7D_FourByteInt
module    function nf90_get_var_1D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_FourByteReal
   end function nf90_get_var_1D_FourByteReal
module    function nf90_get_var_2D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_FourByteReal
   end function nf90_get_var_2D_FourByteReal
module    function nf90_get_var_3D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_FourByteReal
   end function nf90_get_var_3D_FourByteReal
module    function nf90_get_var_4D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_FourByteReal
   end function nf90_get_var_4D_FourByteReal
module    function nf90_get_var_5D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_FourByteReal
   end function nf90_get_var_5D_FourByteReal
module    function nf90_get_var_6D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_FourByteReal
   end function nf90_get_var_6D_FourByteReal
module    function nf90_get_var_7D_FourByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  6, R =  37)), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_FourByteReal
   end function nf90_get_var_7D_FourByteReal
module    function nf90_get_var_1D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_1D_EightByteReal
   end function nf90_get_var_1D_EightByteReal
module    function nf90_get_var_2D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_2D_EightByteReal
   end function nf90_get_var_2D_EightByteReal
module    function nf90_get_var_3D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_3D_EightByteReal
   end function nf90_get_var_3D_EightByteReal
module    function nf90_get_var_4D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_4D_EightByteReal
   end function nf90_get_var_4D_EightByteReal
module    function nf90_get_var_5D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_5D_EightByteReal
   end function nf90_get_var_5D_EightByteReal
module    function nf90_get_var_6D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_6D_EightByteReal
   end function nf90_get_var_6D_EightByteReal
module    function nf90_get_var_7D_EightByteReal(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     real (kind = selected_real_kind(P =  13, R =  307)), dimension(:, :, :, :, :, :, :), &
                                      intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_7D_EightByteReal
   end function nf90_get_var_7D_EightByteReal
module function nf90_put_var_1D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:), intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_1D_EightByteInt
end function nf90_put_var_1D_EightByteInt
module function nf90_put_var_2D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_2D_EightByteInt
end function nf90_put_var_2D_EightByteInt
module function nf90_put_var_3D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_3D_EightByteInt
end function nf90_put_var_3D_EightByteInt
module function nf90_put_var_4D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_4D_EightByteInt
end function nf90_put_var_4D_EightByteInt
module function nf90_put_var_5D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_5D_EightByteInt
end function nf90_put_var_5D_EightByteInt
module function nf90_put_var_6D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_6D_EightByteInt
end function nf90_put_var_6D_EightByteInt
module function nf90_put_var_7D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :, :, :), &
       intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_put_var_7D_EightByteInt
end function nf90_put_var_7D_EightByteInt
module function nf90_get_var_1D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_1D_EightByteInt
end function nf90_get_var_1D_EightByteInt
module function nf90_get_var_2D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_2D_EightByteInt
end function nf90_get_var_2D_EightByteInt
module function nf90_get_var_3D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_3D_EightByteInt
end function nf90_get_var_3D_EightByteInt
module function nf90_get_var_4D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_4D_EightByteInt
end function nf90_get_var_4D_EightByteInt
module function nf90_get_var_5D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_5D_EightByteInt
end function nf90_get_var_5D_EightByteInt
module function nf90_get_var_6D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_6D_EightByteInt
end function nf90_get_var_6D_EightByteInt
module function nf90_get_var_7D_EightByteInt(ncid, varid, values, start, count, stride, map)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), dimension(:, :, :, :, :, :, :), &
       intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start, count, stride, map
  integer :: nf90_get_var_7D_EightByteInt
end function nf90_get_var_7D_EightByteInt
module function nf90_put_var_EightByteInt(ncid, varid, values, start)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), intent( in) :: values
  integer, dimension(:), optional, intent( in) :: start
  integer :: nf90_put_var_EightByteInt
end function nf90_put_var_EightByteInt
module function nf90_get_var_EightByteInt(ncid, varid, values, start)
  integer, intent( in) :: ncid, varid
  integer (kind = selected_int_kind(18)), intent(out) :: values
  integer, dimension(:), optional, intent( in) :: start
  integer :: nf90_get_var_EightByteInt
end function nf90_get_var_EightByteInt
module   function nf90_create_par(path, cmode, comm, info, ncid, cache_size, &
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
  end function nf90_create_par
module   function nf90_open_par(path, cmode, comm, info, ncid, cache_size, &
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
  end function nf90_open_par
module   function nf90_var_par_access(ncid, varid, access)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: access
    integer :: nf90_var_par_access
  end function nf90_var_par_access
module   function nf90_inq_ncid(ncid, name, grp_ncid)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: name
    integer, intent(out) :: grp_ncid
    integer :: nf90_inq_ncid
  end function nf90_inq_ncid
module   function nf90_inq_grps(ncid, numgrps, ncids)
    integer, intent(in) :: ncid
    integer, intent(out) :: numgrps
    integer, dimension(:), intent(out) :: ncids
    integer :: nf90_inq_grps
  end function nf90_inq_grps
module   function nf90_inq_grpname_len(ncid, len)
    integer, intent(in) :: ncid
    integer, intent(out) :: len
    integer :: nf90_inq_grpname_len
  end function nf90_inq_grpname_len
module   function nf90_inq_grp_ncid(ncid, name, grpid)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: name
    integer, intent(out) :: grpid
    integer :: nf90_inq_grp_ncid
  end function nf90_inq_grp_ncid
module   function nf90_inq_grp_full_ncid(ncid, full_name, grpid)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: full_name
    integer, intent(out) :: grpid
    integer :: nf90_inq_grp_full_ncid
  end function nf90_inq_grp_full_ncid
module   function nf90_inq_grp_parent(ncid, parent_ncid)
    integer, intent(in) :: ncid
    integer, intent(out) :: parent_ncid
    integer :: nf90_inq_grp_parent
  end function nf90_inq_grp_parent
module   function nf90_inq_grpname(ncid, name)
    integer, intent(in) :: ncid
    character (len = *), intent(out) :: name
    integer :: nf90_inq_grpname
  end function nf90_inq_grpname
module   function nf90_inq_grpname_full(ncid, len, name)
    integer, intent(in) :: ncid
    integer, intent(out) :: len
    character (len = *), intent(out) :: name
    integer :: nf90_inq_grpname_full
  end function nf90_inq_grpname_full
module   function nf90_inq_varids(ncid, nvars, varids)
    integer, intent(in) :: ncid
    integer, intent(out) :: nvars
    integer, dimension(:), intent(out) :: varids
    integer :: nf90_inq_varids
  end function nf90_inq_varids
module   function nf90_inq_dimids(ncid, ndims, dimids, include_parents)
    integer, intent(in) :: ncid
    integer, intent(out) :: ndims
    integer, dimension(:), intent(out) :: dimids
    integer, intent(out) :: include_parents
    integer :: nf90_inq_dimids
  end function nf90_inq_dimids
module   function nf90_inq_typeids(ncid, ntypes, typeids)
    integer, intent(in) :: ncid
    integer, optional, intent(out) :: ntypes
    integer, dimension(:), optional, intent(out) :: typeids
    integer :: nf90_inq_typeids
  end function nf90_inq_typeids
module   function nf90_inq_typeid(ncid, name, typeid)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: name
    integer, optional, intent(out) :: typeid
    integer :: nf90_inq_typeid
  end function nf90_inq_typeid
module   function nf90_def_grp(parent_ncid, name, new_ncid)
    integer, intent(in) :: parent_ncid
    character (len = *), intent(in) :: name
    integer, intent(out) :: new_ncid
    integer :: nf90_def_grp
  end function nf90_def_grp
module   function nf90_rename_grp(grpid, name)
    integer, intent(in) :: grpid
    character (len = *), intent(in) :: name
    integer :: nf90_rename_grp
  end function nf90_rename_grp
module   function nf90_def_compound(ncid, size, name, typeid)
    integer, intent(in) :: ncid
    integer, intent(in) :: size
    character (len = *), intent(in) :: name
    integer, intent(out) :: typeid
    integer :: nf90_def_compound
  end function nf90_def_compound
module   function nf90_insert_compound(ncid, xtype, name, offset, field_typeid)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(in) :: name
    integer, intent(in) :: offset
    integer, intent(in) :: field_typeid
    integer :: nf90_insert_compound
  end function nf90_insert_compound
module   function nf90_insert_array_compound(ncid, xtype, name, offset, field_typeid, &
       ndims, dim_sizes)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(in) :: name
    integer, intent(in) :: offset
    integer, intent(in) :: field_typeid
    integer, intent(in) :: ndims
    integer, intent(in) :: dim_sizes
    integer :: nf90_insert_array_compound
  end function nf90_insert_array_compound
module   function nf90_inq_type(ncid, xtype, name, size)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: size
    integer :: nf90_inq_type
  end function nf90_inq_type
module   function nf90_inq_compound(ncid, xtype, name, size, nfields)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: size
    integer, intent(out) :: nfields
    integer :: nf90_inq_compound
  end function nf90_inq_compound
module   function nf90_inq_compound_name(ncid, xtype, name)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer :: nf90_inq_compound_name
  end function nf90_inq_compound_name
module   function nf90_inq_compound_size(ncid, xtype, size)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(out) :: size
    integer :: nf90_inq_compound_size
  end function nf90_inq_compound_size
module   function nf90_inq_compound_nfields(ncid, xtype, nfields)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(out) :: nfields
    integer :: nf90_inq_compound_nfields
  end function nf90_inq_compound_nfields
module   function nf90_inq_compound_field(ncid, xtype, fieldid, name, offset, &
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
  end function nf90_inq_compound_field
module   function nf90_inq_compound_fieldname(ncid, xtype, fieldid, name)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    character (len = *), intent(out) :: name
    integer :: nf90_inq_compound_fieldname
  end function nf90_inq_compound_fieldname
module   function nf90_inq_compound_fieldindex(ncid, xtype, name, fieldid)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(in) :: name
    integer, intent(out) :: fieldid
    integer :: nf90_inq_compound_fieldindex
  end function nf90_inq_compound_fieldindex
module   function nf90_inq_compound_fieldoffset(ncid, xtype, fieldid, offset)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    integer, intent(out) :: offset
    integer :: nf90_inq_compound_fieldoffset
  end function nf90_inq_compound_fieldoffset
module   function nf90_inq_compound_fieldtype(ncid, xtype, fieldid, field_typeid)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    integer, intent(out) :: field_typeid
    integer :: nf90_inq_compound_fieldtype
  end function nf90_inq_compound_fieldtype
module   function nf90_inq_compound_fieldndims(ncid, xtype, fieldid, ndims)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    integer, intent(out) :: ndims
    integer :: nf90_inq_compound_fieldndims
  end function nf90_inq_compound_fieldndims
module   function nf90_inq_cmp_fielddim_sizes(ncid, xtype, fieldid, dim_sizes)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: fieldid
    integer, intent(out) :: dim_sizes
    integer :: nf90_inq_cmp_fielddim_sizes
  end function nf90_inq_cmp_fielddim_sizes
module   function nf90_def_vlen(ncid, name, base_typeid, xtypeid)
    integer, intent(in) :: ncid
    character (len = *), intent(in) :: name
    integer, intent(in) :: base_typeid
    integer, intent(out) :: xtypeid
    integer :: nf90_def_vlen
  end function nf90_def_vlen
module   function nf90_inq_vlen(ncid, xtype, name, datum_size, base_nc_type)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: datum_size
    integer, intent(out) :: base_nc_type
    integer :: nf90_inq_vlen
  end function nf90_inq_vlen
module   function nf90_free_vlen(vl)
    character (len = *), intent(in) :: vl
    integer :: nf90_free_vlen
  end function nf90_free_vlen
module   function nf90_def_enum(ncid, base_typeid, name, typeid)
    integer, intent(in) :: ncid
    integer, intent(in) :: base_typeid
    character (len = *), intent(in) :: name
    integer, intent(out) :: typeid
    integer :: nf90_def_enum
  end function nf90_def_enum
module   function nf90_inq_user_type(ncid, xtype, name, size, base_typeid, nfields, class)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: size
    integer, intent(out) :: base_typeid
    integer, intent(out) :: nfields
    integer, intent(out) :: class
    integer :: nf90_inq_user_type
  end function nf90_inq_user_type
module   function nf90_insert_enum(ncid, xtype, name, value)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(in) :: name
    integer, intent(in) :: value
    integer :: nf90_insert_enum
  end function nf90_insert_enum
module   function nf90_inq_enum(ncid, xtype, name, base_nc_type, base_size, num_members)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: base_nc_type
    integer, intent(out) :: base_size
    integer, intent(out) :: num_members
    integer :: nf90_inq_enum
  end function nf90_inq_enum
module   function nf90_inq_enum_member(ncid, xtype, idx, name, value)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: idx
    character (len = *), intent(out) :: name
    integer, intent(in) :: value
    integer :: nf90_inq_enum_member
  end function nf90_inq_enum_member
module   function nf90_inq_enum_ident(ncid, xtype, value, idx)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    integer, intent(in) :: value
    integer, intent(out) :: idx
    integer :: nf90_inq_enum_ident
  end function nf90_inq_enum_ident
module   function nf90_def_opaque(ncid, size, name, xtype)
    integer, intent(in) :: ncid
    integer, intent(in) :: size
    character (len = *), intent(in) :: name
    integer, intent(out) :: xtype
    integer :: nf90_def_opaque
  end function nf90_def_opaque
module   function nf90_inq_opaque(ncid, xtype, name, size)
    integer, intent(in) :: ncid
    integer, intent(in) :: xtype
    character (len = *), intent(out) :: name
    integer, intent(out) :: size
    integer :: nf90_inq_opaque
  end function nf90_inq_opaque
module   function nf90_def_var_chunking(ncid, varid, contiguous, chunksizes)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: contiguous
    integer, dimension(:), intent(in) :: chunksizes
    integer :: nf90_def_var_chunking
  end function nf90_def_var_chunking
module   function nf90_def_var_deflate(ncid, varid, shuffle, deflate, deflate_level)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: shuffle
    integer, intent(in) :: deflate
    integer, intent(in) :: deflate_level
    integer :: nf90_def_var_deflate
  end function nf90_def_var_deflate
module   function nf90_def_var_szip(ncid, varid, options_mask, pixels_per_block)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: options_mask
    integer, intent(in) :: pixels_per_block
    integer :: nf90_def_var_szip
  end function nf90_def_var_szip
module   function nf90_def_var_fletcher32(ncid, varid, fletcher32)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: fletcher32
    integer :: nf90_def_var_fletcher32
  end function nf90_def_var_fletcher32
module   function nf90_inq_var_chunking(ncid, varid, contiguous, chunksizes)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: contiguous
    integer, dimension(:), intent(out) :: chunksizes
    integer :: nf90_inq_var_chunking
  end function nf90_inq_var_chunking
module   function nf90_inq_var_deflate(ncid, varid, shuffle, deflate, deflate_level)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: shuffle
    integer, intent(out) :: deflate
    integer, intent(out) :: deflate_level
    integer :: nf90_inq_var_deflate
  end function nf90_inq_var_deflate
module   function nf90_inq_var_szip(ncid, varid, options_mask, pixels_per_block)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: options_mask
    integer, intent(out) :: pixels_per_block
    integer :: nf90_inq_var_szip
  end function nf90_inq_var_szip
module   function nf90_inq_var_fletcher32(ncid, varid, fletcher32)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: fletcher32
    integer :: nf90_inq_var_fletcher32
  end function nf90_inq_var_fletcher32
module   function nf90_def_var_endian(ncid, varid, endian)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: endian
    integer :: nf90_def_var_endian
  end function nf90_def_var_endian
module   function nf90_inq_var_endian(ncid, varid, endian)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: endian
    integer :: nf90_inq_var_endian
  end function nf90_inq_var_endian
module   function nf90_def_var_filter(ncid, varid, filterid, nparams, params)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: filterid
    integer, intent(in) :: nparams
    integer, intent(in) :: params(*)
    integer :: nf90_def_var_filter
  end function nf90_def_var_filter
module   function nf90_inq_var_filter(ncid, varid, filterid, nparams, params)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(out) :: filterid
    integer, intent(out) :: nparams
    integer, dimension(:), intent(out) :: params
    integer :: nf90_inq_var_filter
  end function nf90_inq_var_filter
module   function nf90_def_var_fill_OneByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    integer(kind=selected_int_kind(2)), intent(in) :: fill
    integer :: nf90_def_var_fill_OneByteInt
  end function nf90_def_var_fill_OneByteInt
module   function nf90_def_var_fill_TwoByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    integer(kind=selected_int_kind(4)), intent(in) :: fill
    integer :: nf90_def_var_fill_TwoByteInt
  end function nf90_def_var_fill_TwoByteInt
module   function nf90_def_var_fill_FourByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    integer(kind=selected_int_kind(9)), intent(in) :: fill
    integer :: nf90_def_var_fill_FourByteInt
  end function nf90_def_var_fill_FourByteInt
module   function nf90_def_var_fill_EightByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    integer(kind=selected_int_kind(18)), intent(in) :: fill
    integer :: nf90_def_var_fill_EightByteInt
  end function nf90_def_var_fill_EightByteInt
module   function nf90_def_var_fill_FourByteReal(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    real(kind=selected_real_kind(P =  6, R =  37)), intent(in) :: fill
    integer :: nf90_def_var_fill_FourByteReal
  end function nf90_def_var_fill_FourByteReal
module   function nf90_def_var_fill_EightByteReal(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(in) :: no_fill
    real(kind=selected_real_kind(P =  13, R =  307)), intent(in) :: fill
    integer :: nf90_def_var_fill_EightByteReal
  end function nf90_def_var_fill_EightByteReal
module   function nf90_inq_var_fill_OneByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    integer(kind=selected_int_kind(2)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_OneByteInt
  end function nf90_inq_var_fill_OneByteInt
module   function nf90_inq_var_fill_TwoByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    integer(kind=selected_int_kind(4)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_TwoByteInt
  end function nf90_inq_var_fill_TwoByteInt
module   function nf90_inq_var_fill_FourByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    integer(kind=selected_int_kind(9)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_FourByteInt
  end function nf90_inq_var_fill_FourByteInt
module   function nf90_inq_var_fill_EightByteInt(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    integer(kind=selected_int_kind(18)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_EightByteInt
  end function nf90_inq_var_fill_EightByteInt
module   function nf90_inq_var_fill_FourByteReal(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    real(kind=selected_real_kind(P =  6, R =  37)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_FourByteReal
  end function nf90_inq_var_fill_FourByteReal
module   function nf90_inq_var_fill_EightByteReal(ncid, varid, no_fill, fill)
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    integer, intent(inout) :: no_fill
    real(kind=selected_real_kind(P =  13, R =  307)), intent(inout) :: fill
    integer :: nf90_inq_var_fill_EightByteReal
  end function nf90_inq_var_fill_EightByteReal
module   function nf90_put_att_any(ncid, varid, name, typeid, length, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer, intent( in) :: typeid, length
    character(len = *), intent( in) :: values
    integer :: nf90_put_att_any
  end function nf90_put_att_any
module   function nf90_get_att_any(ncid, varid, name, length, values)
    integer, intent( in) :: ncid, varid
    character(len = *), intent( in) :: name
    integer, intent( in) :: length
    character(len = *), intent( in) :: values
    integer :: nf90_get_att_any
  end function nf90_get_att_any
module    function nf90_put_var_any(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), intent( in) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_put_var_any
   end function nf90_put_var_any
module    function nf90_get_var_any(ncid, varid, values, start, count, stride, map)
     integer, intent( in) :: ncid, varid
     character (len = *), intent(out) :: values
     integer, dimension(:), optional, intent( in) :: start, count, stride, map
     integer :: nf90_get_var_any
   end function nf90_get_var_any
end interface
end module netcdf
