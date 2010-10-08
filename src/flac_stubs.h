
#include <FLAC/format.h>
#include <FLAC/metadata.h>
#include <FLAC/stream_decoder.h>
#include <FLAC/stream_encoder.h>

#include <caml/mlvalues.h>

#define Val_none Val_int(0)
#define Some_val(v) Field(v,0)
value flac_Val_some(value v);

/* Decoder */

typedef struct ocaml_flac_decoder_callbacks {
  /* This is used for ogg callbacks. */
  void *private;
  /* This is used for callback from caml. */ 
  value callbacks;
  value tmp;
  FLAC__StreamMetadata_StreamInfo *info;
  FLAC__StreamMetadata *meta;
} ocaml_flac_decoder_callbacks;

typedef struct ocaml_flac_decoder {
  FLAC__StreamDecoder *decoder ;
  ocaml_flac_decoder_callbacks callbacks;
} ocaml_flac_decoder;

void finalize_decoder(value dec);

/* Caml abstract value containing the decoder. */
#define Decoder_val(v) (*((ocaml_flac_decoder**)Data_custom_val(v)))

void dec_metadata_callback(const FLAC__StreamDecoder *decoder,
                       const FLAC__StreamMetadata *metadata,
                       void *client_data);

FLAC__StreamDecoderWriteStatus dec_write_callback(const FLAC__StreamDecoder *decoder,
                                              const FLAC__Frame *frame,
                                              const FLAC__int32 * const buffer[],
                                              void *client_data);

void dec_error_callback(const FLAC__StreamDecoder *decoder,
                    FLAC__StreamDecoderErrorStatus status,
                    void *client_data);

/* Encoder */

typedef struct ocaml_flac_encoder_callbacks {
  /* This is used by the caml encoder. */
  value callbacks;
  value tmp;
  /* This is used by the ogg encoder. */
  void *private;
} ocaml_flac_encoder_callbacks;

typedef struct ocaml_flac_encoder {
  FLAC__StreamEncoder *encoder ;
  ocaml_flac_encoder_callbacks callbacks;
} ocaml_flac_encoder;

/* Caml abstract value containing the decoder. */
#define Encoder_val(v) (*((ocaml_flac_encoder**)Data_custom_val(v)))

void finalize_encoder(value dec);
