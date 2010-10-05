
#include <ogg/ogg.h>
#include <memory.h>

#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/custom.h>

#include <ocaml-ogg.h>

#include "flac_stubs.h"

typedef struct ocaml_flac_ogg_private {
  unsigned char *data;
  long bytes;
  long offset;
  value os;
} ocaml_flac_ogg_private;

static void finalize_ogg_decoder(value e)
{
  ocaml_flac_decoder *dec = Decoder_val(e);
  ocaml_flac_ogg_private *p = dec->callbacks.private;
  if (p->data != NULL)
    free(p->data);
  caml_remove_global_root(&p->os);
  free(p);
  finalize_decoder(e); 
}

static struct custom_operations ogg_decoder_ops =
{
  "ocaml_flac_ogg_decoder",
  finalize_ogg_decoder,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* C.f. http://flac.sourceforge.net/ogg_mapping.html */
CAMLprim value ocaml_flac_decoder_check_ogg(value v)
{
  CAMLparam1(v);
  ogg_packet *p = Packet_val(v);
  unsigned char *h = p->packet;
  if (p->bytes < 9 ||
      /* FLAC */
      h[0] != 0x7f ||
      h[1] != 'F' ||
      h[2] != 'L' ||
      h[3] != 'A' ||
      h[4] != 'C')
    CAMLreturn(Val_false);

  CAMLreturn(Val_true);
}

/* libFLAC is monothread so this
 * is run within the main C thread. 
 * 
 * Ogg/flac mapping says: 
 * "each packet corresponds to one FLAC audio frame."
 * and we decode frame by frame, so we only need to push one packet at a 
 * time here. */
static FLAC__StreamDecoderReadStatus ogg_read_callback(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[],
                                   size_t *bytes, void *client_data)
{
  ocaml_flac_decoder_callbacks *callbacks = (ocaml_flac_decoder_callbacks *)client_data ;

  ocaml_flac_ogg_private *h = (ocaml_flac_ogg_private *)callbacks->private;

  if (h->data == NULL)
  {
     /* Grap a new ogg_packet */
     ogg_packet op;
     ogg_stream_state *os = Stream_state_val(h->os);
     if (ogg_stream_packetout(os,&op) == 0)
     {
       caml_enter_blocking_section();
       caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));
     }
     h->data = malloc(op.bytes);
     if (h->data == NULL)
     {
       caml_enter_blocking_section();
       caml_raise_out_of_memory();
     }
     memcpy(h->data,op.packet,op.bytes);
     h->bytes = op.bytes;
     h->offset = 0;
  }

  long len;
  if (h->bytes-h->offset > *bytes)
    len = *bytes;
  else
    len = h->bytes-h->offset;

  memcpy(buffer,h->data + h->offset,len);
  if (len >= h->bytes-h->offset)
  {
     free(h->data);
     h->data = NULL;
     h->bytes = 0;
     h->offset = 0;
  } else
     h->offset = h->offset + len;

  *bytes = len;
  return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
}

CAMLprim value ocaml_flac_decoder_ogg_create(value v, value os)
{
  CAMLparam2(v,os);
  CAMLlocal1(ans);
  ogg_packet *p = Packet_val(v);

  ocaml_flac_ogg_private *priv = malloc(sizeof(ocaml_flac_ogg_private));
  if (priv == NULL)
    caml_raise_out_of_memory();

  priv->data = malloc(p->bytes);
  if (priv->data == NULL)
    caml_raise_out_of_memory(); 
  memcpy(priv->data,p->packet,p->bytes);
  priv->bytes = p->bytes;
  priv->offset = 9;
  caml_register_global_root(&priv->os);
  priv->os = os; 

  // Initialize things
  ocaml_flac_decoder *dec = malloc(sizeof(ocaml_flac_decoder));
  if (dec == NULL)
    caml_raise_out_of_memory();

  dec->decoder = FLAC__stream_decoder_new();
  dec->callbacks.is_caml = 0;
  dec->callbacks.private = (void*)priv;
  dec->callbacks.out_buf = NULL;
  dec->callbacks.info = NULL;
  dec->callbacks.comments = NULL;

  // Accept vorbis comments
  FLAC__stream_decoder_set_metadata_respond(dec->decoder, FLAC__METADATA_TYPE_VORBIS_COMMENT);

  // Intialize decoder
  FLAC__stream_decoder_init_stream(
        dec->decoder,
        ogg_read_callback,
        NULL,
        NULL,
        NULL,
        NULL,
        write_callback,
        metadata_callback,
        error_callback,
        (void *)&dec->callbacks
  );

  // Fill custom value
  ans = caml_alloc_custom(&ogg_decoder_ops, sizeof(ocaml_flac_decoder*), 1, 0);
  Decoder_val(ans) = dec;

  CAMLreturn(ans);
}

CAMLprim value ocaml_flac_decoder_ogg_init(value v)
{
  CAMLparam1(v);
  ocaml_flac_decoder *dec = Decoder_val(v);

  // Process metadata
  caml_enter_blocking_section();
  FLAC__stream_decoder_process_until_end_of_metadata(dec->decoder);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

