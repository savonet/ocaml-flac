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

#include <pthread.h>

#include <FLAC/format.h>
#include <FLAC/metadata.h>
#include <FLAC/stream_decoder.h>
#include <FLAC/stream_encoder.h>

#include <caml/mlvalues.h>

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
value flac_Val_some(value v);

/* Decoder */

typedef struct ocaml_flac_decoder_callbacks {
  /* This is used for callback from caml. */
  value read;
  value seek;
  value tell;
  value length;
  value eof;
  value write;
  FLAC__StreamMetadata_StreamInfo *info;
  FLAC__StreamMetadata *meta;
} ocaml_flac_decoder_callbacks;

typedef struct ocaml_flac_decoder {
  FLAC__StreamDecoder *decoder;
  ocaml_flac_decoder_callbacks callbacks;
} ocaml_flac_decoder;

#define Fill_dec_values(x, c)                                                  \
  {                                                                            \
    CAMLlocal5(_read_cb, _seek_cb, _tell_cb, _length_cb, _eof_cb);             \
    CAMLlocal1(_write_cb);                                                     \
    _read_cb = Field(c, 0);                                                    \
    _seek_cb = Field(c, 1);                                                    \
    _tell_cb = Field(c, 2);                                                    \
    _length_cb = Field(c, 3);                                                  \
    _eof_cb = Field(c, 4);                                                     \
    _write_cb = Field(c, 5);                                                   \
    x->callbacks.read = _read_cb;                                              \
    x->callbacks.seek = _seek_cb;                                              \
    x->callbacks.tell = _tell_cb;                                              \
    x->callbacks.length = _length_cb;                                          \
    x->callbacks.eof = _eof_cb;                                                \
    x->callbacks.write = _write_cb;                                            \
  }

#define Free_dec_values(x)                                                     \
  {                                                                            \
    x->callbacks.read = Val_none;                                              \
    x->callbacks.seek = Val_none;                                              \
    x->callbacks.tell = Val_none;                                              \
    x->callbacks.length = Val_none;                                            \
    x->callbacks.eof = Val_none;                                               \
    x->callbacks.write = Val_none;                                             \
  }

/* Caml abstract value containing the decoder. */
#define Decoder_val(v) (*((ocaml_flac_decoder **)Data_custom_val(v)))

void dec_metadata_callback(const FLAC__StreamDecoder *decoder,
                           const FLAC__StreamMetadata *metadata,
                           void *client_data);

FLAC__StreamDecoderWriteStatus
dec_write_callback(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame,
                   const FLAC__int32 *const buffer[], void *client_data);

void dec_error_callback(const FLAC__StreamDecoder *decoder,
                        FLAC__StreamDecoderErrorStatus status,
                        void *client_data);

/* Encoder */

typedef struct ocaml_flac_encoder_callbacks {
  /* This is used by the caml encoder. */
  value write;
  value seek;
  value tell;
} ocaml_flac_encoder_callbacks;

typedef struct ocaml_flac_encoder {
  FLAC__StreamEncoder *encoder;
  FLAC__StreamMetadata *meta;
  FLAC__int32 **buf;
  FLAC__int32 *lines;
  ocaml_flac_encoder_callbacks callbacks;
} ocaml_flac_encoder;

#define Fill_enc_values(x, c)                                                  \
  {                                                                            \
    CAMLlocal3(_write_cb, _seek_cb, _tell_cb);                                 \
    _write_cb = Field(c, 0);                                                   \
    _seek_cb = Field(c, 1);                                                    \
    _tell_cb = Field(c, 2);                                                    \
    x->callbacks.write = _write_cb;                                            \
    x->callbacks.seek = _seek_cb;                                              \
    x->callbacks.tell = _tell_cb;                                              \
  }

#define Free_enc_values(x)                                                     \
  {                                                                            \
    x->callbacks.write = Val_none;                                             \
    x->callbacks.seek = Val_none;                                              \
    x->callbacks.tell = Val_none;                                              \
  }

/* Caml abstract value containing the decoder. */
#define Encoder_val(v) (*((ocaml_flac_encoder **)Data_custom_val(v)))

value ocaml_flac_encoder_alloc(value comments, value params);

FLAC__StreamEncoderWriteStatus
enc_write_callback(const FLAC__StreamEncoder *encoder,
                   const FLAC__byte buffer[], size_t bytes, unsigned samples,
                   unsigned current_frame, void *client_data);

/* Threads management */
void ocaml_flac_register_thread();
