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
 * Original code from libaudio-flac-decoder-perl.
 *
 * Chunks of this code have been borrowed and influenced 
 * by flac/decode.c and the flac XMMS plugin.
 *
 */


#include <memory.h>
#include <stdint.h>

#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>

#include <FLAC/stream_decoder.h>

typedef struct ocaml_flac_decoder_callbacks {
  value read_f;
  FLAC__int32 **out_buf;
  FLAC__Frame out_frame;
} ocaml_flac_decoder_callbacks;

typedef struct ocaml_flac_decoder {
  FLAC__StreamDecoder *decoder ;
  ocaml_flac_decoder_callbacks callbacks;
} ocaml_flac_decoder;

/* Caml abstract value containing the decoder. */
#define Decoder_val(v) (*((ocaml_flac_decoder**)Data_custom_val(v)))

static void finalize_decoder(value e)
{
  ocaml_flac_decoder *dec = Decoder_val(e);
  FLAC__stream_decoder_delete(dec->decoder);
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
  // TODO: raise exc if this fails.
  int samples = frame->header.blocksize;
  int channels = frame->header.channels;
  int bits_per_sample = frame->header.bits_per_sample;
  int len = sizeof(FLAC__int32)*samples*channels*bits_per_sample/8;
  callbacks->out_buf = malloc(len);
  memcpy(callbacks->out_buf,buffer,len);
  memcpy(&callbacks->out_frame,frame,sizeof(FLAC__Frame));
  return ;
}

#include <stdio.h>

/* start all the callbacks here. */
static void metadata_callback(const FLAC__StreamDecoder *decoder, 
                          const FLAC__StreamMetadata *metadata, 
                          void *client_data)
{
 if (metadata->type == FLAC__METADATA_TYPE_VORBIS_COMMENT)
 {
    // Lazy..
    FLAC__StreamMetadata_VorbisComment meta = metadata->data.vorbis_comment ;
    int i;
    for (i=0; i< meta.num_comments; i++)
    {
      FLAC__StreamMetadata_VorbisComment_Entry elem = meta.comments[i];
      printf("%s\n",elem.entry);
    }
    fflush(stdout);
 }
 if (metadata->type == FLAC__METADATA_TYPE_STREAMINFO)
 {
    printf("Got streaminfo:\n");
    printf("sample rate: %u\n",metadata->data.stream_info.sample_rate);
    printf("channels: %u\n",metadata->data.stream_info.channels);
    printf("total samples: %llu\n",metadata->data.stream_info.total_samples);
    printf("bits per sameple: %u\n",metadata->data.stream_info.bits_per_sample);
    fflush(stdout);
 }
 return ;
}

static void error_callback(const FLAC__StreamDecoder *decoder, 
                           FLAC__StreamDecoderErrorStatus status, 
                           void *client_data)
{
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

static FLAC__StreamDecoderReadStatus read_callback(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], 
                                   size_t *bytes, void *client_data)
{
  ocaml_flac_decoder_callbacks *callbacks = (ocaml_flac_decoder_callbacks *)client_data ;
  
  value ret = caml_callback(callbacks->read_f,Val_int(*bytes));
  /* TODO: 
  value ret = caml_callback_exn(callbacks->read_f,Val_int(*bytes));
  if (Is_exception_result(ret)) {
    //For now
    return FLAC__STREAM_DECODER_READ_STATUS_ABORT;
  } */
  char *data = String_val(Field(ret,0));
  int len = Int_val(Field(ret,1));
  memcpy(buffer,data,len);
  return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
}

static FLAC__StreamDecoderWriteStatus write_callback(const FLAC__StreamDecoder *decoder, 
                                                     const FLAC__Frame *frame, 
                                                     const FLAC__int32 * const buffer[], 
                                                     void *client_data)
{
  ocaml_flac_decoder_callbacks *callbacks = (ocaml_flac_decoder_callbacks *)client_data ;

  cpy_out_buf(callbacks,buffer,frame);
  return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

CAMLprim value ocaml_flac_decoder_create(value read_func, value seek_func, value tell_func)
{
  CAMLparam1(read_func);
  CAMLlocal1(ans);
  
  // Initialize things
  //TODO: raise exc if this fails
  ocaml_flac_decoder *dec = malloc(sizeof(ocaml_flac_decoder));
  dec->decoder = FLAC__stream_decoder_new();
  dec->callbacks.read_f = read_func;
  caml_register_global_root(&read_func);
  dec->callbacks.out_buf = NULL;
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
  FLAC__stream_decoder_process_until_end_of_metadata(dec->decoder);

  // Fill custom value
  ans = caml_alloc_custom(&decoder_ops, sizeof(ocaml_flac_decoder*), 1, 0);
  Decoder_val(ans) = dec;
  CAMLreturn(ans);
}

// TODO remove this !
CAMLprim value ocaml_flac_decoder_is_eos(value d)
{
  CAMLparam1(d);
  ocaml_flac_decoder *dec = Decoder_val(d);
  if (FLAC__stream_decoder_get_state(dec->decoder) == FLAC__STREAM_DECODER_END_OF_STREAM)
    CAMLreturn(Val_true);

  CAMLreturn(Val_false);
}


CAMLprim value ocaml_flac_decoder_read(value d)
{
  CAMLparam1(d);
  CAMLlocal1(ans);

  // This only work for S16LE (for now)
  ocaml_flac_decoder *dec = Decoder_val(d);
  ocaml_flac_decoder_callbacks *callbacks = &dec->callbacks;

  // Process one frame
  FLAC__stream_decoder_process_single(dec->decoder);

  // Alloc array
  int channels = callbacks->out_frame.header.channels;
  int samples = callbacks->out_frame.header.blocksize;
  ans = caml_alloc_tuple(channels);

  int c,i;
  for (c = 0; c < channels; c++)
    Store_field(ans, c, caml_alloc(samples * Double_wosize, Double_array_tag));

  for (c = 0; c < channels; c++)
    for (i = 0; i < samples; i++)
      Store_double_field(Field(ans, c), i, callbacks->out_buf[c][i]);

  CAMLreturn(ans);
}

CAMLprim value ocaml_flac_decoder_read_pcm(value d)
{
  CAMLparam1(d);
  CAMLlocal1(ans);

  // This only work for S16LE (for now)
  ocaml_flac_decoder *dec = Decoder_val(d);
  ocaml_flac_decoder_callbacks *callbacks = &dec->callbacks;

  // Process one frame
  FLAC__stream_decoder_process_single(dec->decoder);

  // Alloc string
  int channels = callbacks->out_frame.header.channels;
  int samples = callbacks->out_frame.header.blocksize;
  // S16LE
  ans = caml_alloc_string(channels*samples*2);
  int16_t *pcm = (int16_t *)String_val(ans);

  int c,i;
  for (i = 0; i < samples; i++)
    for (c = 0; c < channels; c++)
      pcm[i*channels+c] = callbacks->out_buf[c][i];

  CAMLreturn(ans);
}

#if 0

CAMLprim value ocaml_flac_decoder_channels (value d)
{

}

CAMLprim value ocaml_flac_decoder_bits_per_sample (value d)
{

}

CAMLprim value ocaml_flac_decoder_sample_rate (value d)
{

}

CAMLprim value ocaml_flac_decoder_raw_seek (value d, value _pos, value _whence)
{

}

FLAC__uint64
raw_tell (obj)

sample_seek (obj, sample)

time_seek (obj, seconds)

time_tell (obj)

#endif

