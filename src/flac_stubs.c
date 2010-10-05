/* This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * Chunks of this code have been borrowed and influenced 
 * by flac/decode.c and the flac XMMS plugin.
 *
 */


#include <memory.h>
#include <stdint.h>

#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include "flac_stubs.h"

/* polymorphic variant utility macros */
#define decl_var(x) static value var_##x
#define import_var(x) var_##x = caml_hash_variant(#x)
#define get_var(x) var_##x

/* cached polymorphic variants */
decl_var(Search_for_metadata);
decl_var(Read_metadata);
decl_var(Search_for_frame_sync);
decl_var(Read_frame);
decl_var(End_of_stream);
decl_var(Ogg_error);
decl_var(Seek_error);
decl_var(Aborted);
decl_var(Memory_allocation_error);
decl_var(Uninitialized);
decl_var(Unknown);

static value val_of_state(int s) {
  switch (s)
    {
    case FLAC__STREAM_DECODER_SEARCH_FOR_METADATA:
      return get_var(Search_for_metadata);
    case FLAC__STREAM_DECODER_READ_METADATA:
      return get_var(Read_metadata);
    case FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC:
      return get_var(Search_for_frame_sync);
    case FLAC__STREAM_DECODER_READ_FRAME:
      return get_var(Read_frame);
    case FLAC__STREAM_DECODER_END_OF_STREAM:
      return get_var(End_of_stream);
    case FLAC__STREAM_DECODER_OGG_ERROR:
      return get_var(Ogg_error);
    case FLAC__STREAM_DECODER_SEEK_ERROR:
      return get_var(Seek_error);
    case FLAC__STREAM_DECODER_ABORTED:
      return get_var(Aborted);
    case FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR:
      return get_var(Memory_allocation_error);
    case FLAC__STREAM_DECODER_UNINITIALIZED:
      return get_var(Uninitialized);
    default:
      return get_var(Unknown);
    }
}

static value exn_of_error(FLAC__StreamDecoderErrorStatus e) {
  switch (e)
    {
    case FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC:
      return *caml_named_value("flac_dec_exn_lost_sync");
    case FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER:
      return *caml_named_value("flac_dec_exn_bad_header");
    case FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH:
      return *caml_named_value("flac_dec_exn_crc_mismatch");
    case FLAC__STREAM_DECODER_ERROR_STATUS_UNPARSEABLE_STREAM:
      return *caml_named_value("flac_dec_exn_unparseable_stream");
    default:
      return *caml_named_value("flac_exn_internal");
    }
}

/* initialize the module */
CAMLprim value ocaml_flac_stubs_initialize(value unit)
{
  CAMLparam0();
  /* initialize polymorphic variants */
  import_var(Search_for_metadata);
  import_var(Read_metadata);
  import_var(Search_for_frame_sync);
  import_var(Read_frame);
  import_var(End_of_stream);
  import_var(Ogg_error);
  import_var(Seek_error);
  import_var(Aborted);
  import_var(Memory_allocation_error);
  import_var(Uninitialized);
  import_var(Unknown);
  CAMLreturn(Val_unit);
}

/* Caml abstract value containing the decoder. */
#define Decoder_val(v) (*((ocaml_flac_decoder**)Data_custom_val(v)))

void finalize_decoder(value e)
{
  ocaml_flac_decoder *dec = Decoder_val(e);
  FLAC__stream_decoder_delete(dec->decoder);
  if (dec->callbacks.info != NULL)
    free(dec->callbacks.info);
  if (dec->callbacks.out_buf != NULL)
    free(dec->callbacks.out_buf);
  if (dec->callbacks.comments != NULL)
    FLAC__metadata_object_delete(dec->callbacks.comments);
  if (dec->callbacks.is_caml == 1)
    caml_remove_global_root(&dec->callbacks.read_f);
  free(dec);
}

static struct custom_operations decoder_ops =
{
  "ocaml_flac_decoder",
  finalize_decoder,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static void cpy_out_buf(ocaml_flac_decoder_callbacks *callbacks, 
                        const FLAC__int32 * const buffer[],  
                        const FLAC__Frame *frame)
{
  if (callbacks->out_buf != NULL) 
    // free previous memory pointer
    free(callbacks->out_buf);
  int samples = frame->header.blocksize;
  int channels = frame->header.channels;
  int bits_per_sample = frame->header.bits_per_sample;
  int len = sizeof(FLAC__int32)*samples*channels*bits_per_sample/8;
  callbacks->out_buf = malloc(len);
  if (callbacks->out_buf == NULL)
  {
    // This callback is run in non-blocking mode
    caml_leave_blocking_section();
    caml_raise_out_of_memory();
  }
  memcpy(callbacks->out_buf,buffer,len);
  memcpy(&callbacks->out_frame,frame,sizeof(FLAC__Frame));
  return ;
}

/* start all the callbacks here. */
void metadata_callback(const FLAC__StreamDecoder *decoder, 
                       const FLAC__StreamMetadata *metadata, 
                       void *client_data)
{
 ocaml_flac_decoder_callbacks *callbacks = (ocaml_flac_decoder_callbacks *)client_data ;
 switch (metadata->type) {
    case FLAC__METADATA_TYPE_STREAMINFO:
      if (callbacks->info != NULL)
        free(callbacks->info);
      callbacks->info = malloc(sizeof(FLAC__StreamMetadata_StreamInfo));
      if (callbacks->info == NULL)
      {
        // This callback is run in non-blocking mode
        caml_leave_blocking_section();
        caml_raise_out_of_memory();
      }
      memcpy(callbacks->info,&metadata->data.stream_info,sizeof(FLAC__StreamMetadata_StreamInfo));
      break;
    case FLAC__METADATA_TYPE_VORBIS_COMMENT:
      if (callbacks->comments != NULL)
        FLAC__metadata_object_delete(callbacks->comments) ;
      callbacks->comments = FLAC__metadata_object_clone (metadata);
      if (callbacks->comments == NULL)
      {
        // This callback is run in non-blocking mode
        caml_leave_blocking_section();
        caml_raise_out_of_memory();
      }
    default:
      break;
 }
 return ;
}

void error_callback(const FLAC__StreamDecoder *decoder, 
                    FLAC__StreamDecoderErrorStatus status, 
                           void *client_data)
{
 /* This callback is executed in non-blocking section. */
 caml_leave_blocking_section();
 caml_raise_constant(exn_of_error(status));
 return ;
}

static FLAC__StreamDecoderSeekStatus seek_callback(const FLAC__StreamDecoder *decoder, 
                                                   FLAC__uint64 absolute_byte_offset, 
                                                   void *client_data)
{
 return FLAC__STREAM_DECODER_SEEK_STATUS_UNSUPPORTED;
}

static FLAC__StreamDecoderTellStatus tell_callback(const FLAC__StreamDecoder *decoder, 
                                                   FLAC__uint64 *absolute_byte_offset, 
                                                   void *client_data)
{
 return FLAC__STREAM_DECODER_TELL_STATUS_UNSUPPORTED;
}

static FLAC__StreamDecoderLengthStatus length_callback(const FLAC__StreamDecoder *decoder, 
                                                       FLAC__uint64 *stream_length, 
                                                       void *client_data)
{
  return FLAC__STREAM_DECODER_LENGTH_STATUS_UNSUPPORTED;
}

static FLAC__bool eof_callback(const FLAC__StreamDecoder *decoder, void *client_data)
{
 return false;
}

/* libFLAC is monothread so this
 * is run within the main C thread. */
static FLAC__StreamDecoderReadStatus read_callback(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], 
                                   size_t *bytes, void *client_data)
{
  value ret;
  ocaml_flac_decoder_callbacks *callbacks = (ocaml_flac_decoder_callbacks *)client_data ;

  caml_leave_blocking_section(); 
  caml_register_global_root(&ret);
  ret = caml_callback(callbacks->read_f,Val_int(*bytes));
  char *data = String_val(Field(ret,0));
  int len = Int_val(Field(ret,1));
  memcpy(buffer,data,len);
  *bytes = len;
  caml_remove_global_root(&ret);
  caml_enter_blocking_section();

  return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
}

FLAC__StreamDecoderWriteStatus write_callback(const FLAC__StreamDecoder *decoder, 
                                              const FLAC__Frame *frame, 
                                              const FLAC__int32 * const buffer[], 
                                              void *client_data)
{
  ocaml_flac_decoder_callbacks *callbacks = (ocaml_flac_decoder_callbacks *)client_data ;

  cpy_out_buf(callbacks,buffer,frame);
  return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

CAMLprim value ocaml_flac_decoder_create(value read_func)
{
  CAMLparam1(read_func);
  CAMLlocal1(ans);
  
  // Initialize things
  ocaml_flac_decoder *dec = malloc(sizeof(ocaml_flac_decoder));
  if (dec == NULL)
    caml_raise_out_of_memory();

  dec->decoder = FLAC__stream_decoder_new();
  caml_register_global_root(&dec->callbacks.read_f);
  dec->callbacks.is_caml = 1;
  dec->callbacks.read_f = read_func;
  dec->callbacks.private = NULL;
  dec->callbacks.out_buf = NULL;
  dec->callbacks.info = NULL;
  dec->callbacks.comments = NULL;

  // Accept vorbis comments
  FLAC__stream_decoder_set_metadata_respond(dec->decoder, FLAC__METADATA_TYPE_VORBIS_COMMENT);

  // Intialize decoder
  FLAC__stream_decoder_init_stream(
        dec->decoder,
        read_callback,
        seek_callback,
        tell_callback,
        length_callback,
        eof_callback,
        write_callback,
        metadata_callback,
        error_callback,
        (void *)&dec->callbacks
  );

  // Process metadata
  caml_enter_blocking_section();
  FLAC__stream_decoder_process_until_end_of_metadata(dec->decoder);
  caml_leave_blocking_section();

  // Fill custom value
  ans = caml_alloc_custom(&decoder_ops, sizeof(ocaml_flac_decoder*), 1, 0);
  Decoder_val(ans) = dec;
  CAMLreturn(ans);
}

CAMLprim value ocaml_flac_decoder_state(value d)
{
  CAMLparam1(d);
  ocaml_flac_decoder *dec = Decoder_val(d);
  CAMLreturn(val_of_state(FLAC__stream_decoder_get_state(dec->decoder)));
}

CAMLprim value ocaml_flac_decoder_info(value d)
{
  CAMLparam1(d);
  CAMLlocal2(v,s);
  ocaml_flac_decoder *dec = Decoder_val(d);
  FLAC__StreamMetadata_StreamInfo *info = dec->callbacks.info;
  if (info == NULL)
    caml_raise_constant(*caml_named_value("flac_exn_internal"));

  v = caml_alloc_tuple(5);
  Store_field(v,0,Val_int(info->sample_rate));
  Store_field(v,1,Val_int(info->channels));
  Store_field(v,2,Val_int(info->bits_per_sample));
  Store_field(v,3,caml_copy_int64(info->total_samples));
  s = caml_alloc_string(16);
  memcpy(String_val(s),info->md5sum,16);
  Store_field(v,4,s);
  CAMLreturn(v);
}

CAMLprim value ocaml_flac_decoder_comments(value d)
{
  CAMLparam1(d);
  CAMLlocal2(v, ans);
  ocaml_flac_decoder *dec = Decoder_val(d);
  FLAC__StreamMetadata *comments = dec->callbacks.comments;
  int i;
  if (comments == NULL)
    caml_raise_constant(*caml_named_value("flac_exn_internal"));

  FLAC__StreamMetadata_VorbisComment coms = comments->data.vorbis_comment;

  ans = caml_alloc_tuple(2);
  // First comment is vendor string
  Store_field(ans,0,caml_copy_string((char *)coms.vendor_string.entry));
  // Now the other metadata
  v = caml_alloc_tuple(coms.num_comments);
  for (i = 0; i < coms.num_comments; i++)
    Store_field(v,i,caml_copy_string((char *)coms.comments[i].entry));
  Store_field(ans,1,v); 

  CAMLreturn(ans);
}

static inline double sample_to_double(FLAC__int32 x, unsigned bps)
{
  switch (bps) 
  {
    /* 8 bit PCM samples are usually 
     * unsigned. */
    case 8:
      return (((double)x-INT8_MAX)/INT8_MAX);
    case 16:
      return (((double)x)/INT16_MAX);
    default:
      return (((double)x)/INT32_MAX);
  }
}

CAMLprim value ocaml_flac_decoder_read(value d)
{
  CAMLparam1(d);
  CAMLlocal1(ans);

  ocaml_flac_decoder *dec = Decoder_val(d);
  ocaml_flac_decoder_callbacks *callbacks = &dec->callbacks;

  // Process one frame
  caml_enter_blocking_section();
  FLAC__stream_decoder_process_single(dec->decoder);
  caml_leave_blocking_section();

  // Alloc array
  int channels = callbacks->out_frame.header.channels;
  int samples = callbacks->out_frame.header.blocksize;
  unsigned bps = callbacks->out_frame.header.bits_per_sample;
  ans = caml_alloc_tuple(channels);

  int c,i;
  for (c = 0; c < channels; c++)
    Store_field(ans, c, caml_alloc(samples * Double_wosize, Double_array_tag));

  for (c = 0; c < channels; c++)
    for (i = 0; i < samples; i++)
      Store_double_field(Field(ans, c), i, sample_to_double(callbacks->out_buf[c][i],bps));

  CAMLreturn(ans);
}

static inline void sample_to_pcm(FLAC__int32 x, unsigned bps, char *out, int pos)
{
  switch (bps)
  {
    /* 8 bit PCM samples are usually
     * unsigned. */
    case 8:
      ((uint8_t *)out)[pos] = (uint8_t)x;
      break;
    case 16:
      ((int16_t *)out)[pos] = (int16_t)x;
      break;
    default:
      ((int32_t *)out)[pos] = (int32_t)x;
      break;
  }
}


CAMLprim value ocaml_flac_decoder_read_pcm(value d)
{
  CAMLparam1(d);
  CAMLlocal1(ans);

  // This only work for S16LE (for now)
  ocaml_flac_decoder *dec = Decoder_val(d);
  ocaml_flac_decoder_callbacks *callbacks = &dec->callbacks;

  // Process one frame
  caml_enter_blocking_section();
  FLAC__stream_decoder_process_single(dec->decoder);
  caml_leave_blocking_section();

  // Alloc string
  int channels = callbacks->out_frame.header.channels;
  int samples = callbacks->out_frame.header.blocksize;
  unsigned bps = callbacks->out_frame.header.bits_per_sample;
  // S16_LE
  ans = caml_alloc_string(channels*samples*bps/8);
  char *pcm = String_val(ans);

  int c,i;
  for (i = 0; i < samples; i++)
    for (c = 0; c < channels; c++)
      sample_to_pcm(callbacks->out_buf[c][i],bps,pcm,i*channels+c);

  CAMLreturn(ans);
}

