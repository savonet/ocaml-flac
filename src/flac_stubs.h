
#include <FLAC/format.h>
#include <FLAC/metadata.h>
#include <FLAC/stream_decoder.h>

#include <caml/mlvalues.h>

typedef struct ocaml_flac_decoder_callbacks {
  /* This is used for ogg callbacks. */
  void *private;
  int is_caml;
  /* This is used for callback from caml. */ 
  value read_f;
  FLAC__int32 **out_buf;
  FLAC__Frame out_frame;
  FLAC__StreamMetadata_StreamInfo *info;
  FLAC__StreamMetadata *comments;
} ocaml_flac_decoder_callbacks;

typedef struct ocaml_flac_decoder {
  FLAC__StreamDecoder *decoder ;
  ocaml_flac_decoder_callbacks callbacks;
} ocaml_flac_decoder;

void finalize_decoder(value dec);

/* Caml abstract value containing the decoder. */
#define Decoder_val(v) (*((ocaml_flac_decoder**)Data_custom_val(v)))

void metadata_callback(const FLAC__StreamDecoder *decoder,
                       const FLAC__StreamMetadata *metadata,
                       void *client_data);

FLAC__StreamDecoderWriteStatus write_callback(const FLAC__StreamDecoder *decoder,
                                              const FLAC__Frame *frame,
                                              const FLAC__int32 * const buffer[],
                                              void *client_data);

void error_callback(const FLAC__StreamDecoder *decoder,
                    FLAC__StreamDecoderErrorStatus status,
                    void *client_data);


