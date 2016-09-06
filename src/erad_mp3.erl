-module(erad_mp3).

-export([open/1]).
-export([read_frame/1]).

-compile([export_all]).

-type fd() :: any().

-spec open(binary()) -> {'ok', fd()} | {'error', any()}.
open(Filename) ->
  file:open(Filename, ['read', 'raw', 'binary']).

-spec read_frame(fd()) -> {'ok', binary()} | 'eof' | {'error', any()}.
read_frame(File) ->
  case file:read(File, 4) of
    {'ok', Data} ->
      read_frame(File, Data);
    Else ->
      Else
  end.

-spec read_frame(fd(), binary()) -> {'id3' | 'frame', binary()} | 'eof' | {'error', any()}.
read_frame(File, <<16#ff:8,             % 24-31
                   2#111:3,             % 21-23
                   MpegIdMask:2,        % 19-20
                   LayerMask:2,         % 17-18
                   Crc:1,               % 16
                   BitRateMask:4,       % 12-15
                   SampleRateMask:2,    % 10-11
                   Padding:1,           % 9
                   Priv:1,              % 8
                   Mode:2,              % 6-7
                   ModeExt:2,           % 4-5
                   Copyrighted:1,       % 3
                   Original:1,          % 2
                   Emphasis:2           % 0-1
                 >>) ->
  lager:debug("reading frame"),
  MpegId = mpeg_id(MpegIdMask),
  Layer = layer(LayerMask),
  BitRate = bit_rate(MpegId, Layer, BitRateMask),
  SampleRate = sample_rate(MpegId, SampleRateMask),
  FrameSize = frame_size(MpegId, Layer),
  PaddingSize = padding_size(Layer),
  FrameLen = (BitRate * 125 * FrameSize) div SampleRate + Padding * PaddingSize - 4,
  lager:debug("frame size  ~p", [FrameSize]),
  lager:debug("bit rate    ~p", [BitRate]),
  lager:debug("sample rate ~p", [SampleRate]),
  lager:debug("frame len   ~p", [FrameLen]),
  lager:debug("padding     ~p", [Padding]),
  case file:read(File, FrameLen) of
    {'ok', Data} ->
      lager:debug("read ~p", ['_':to_hex(Data)]),
      {'frame', Data};
    Else -> Else
  end;
read_frame(File, <<"TAG+">>) ->
  lager:debug("reading id3v1"),
  case file:read(File, 227 - 4) of
    {'ok', Data} ->
      {'id3', Data};
    Else ->
      Else
  end;
read_frame(File, <<"TAG", Byte:8>>) ->
  lager:debug("reading id3"),
  case file:read(File, 128 - 4) of
    {'ok', Data} ->
      {'id3', <<Byte:8, Data/binary>>};
    Else ->
      Else
  end;
read_frame(File, <<"ID3", VerH:8>>) ->
  lager:debug("reading id3v2"),
  case file:read(File, 10 - 4) of
    {'ok', <<VerL:8, Flags:8, 0:1, S1:7, 0:1, S2:7, 0:1, S3:7, 0:1, S4:7>>} ->
      <<_E:4, Size:24>> = <<S1:7, S2:7, S3:7, S4:7>>,
      lager:debug("id3v2 size is ~p (~p)", [Size, _E]),
      case file:read(File, Size) of
        {'ok', Data} ->
          {'id3', <<VerH:8, VerL:8, Flags:1, Size:4, Data/binary>>};
        Else ->
          Else
      end;
    Else ->
      Else
  end;
read_frame(File, Data) ->
  lager:alert("unmatched ~p", ['_':to_hex(Data)]).

mpeg_id(2#00) -> throw({{'badarg', 'mpeg_id'}, 2.5});
mpeg_id(2#10) -> 2;
mpeg_id(2#11) -> 1.

layer(2#01) -> 3;
layer(2#10) -> 2;
layer(2#11) -> 1.

frame_size(1, 1) -> 384;
frame_size(1, 2) -> 1152;
frame_size(1, 3) -> 1152;
frame_size(2, 1) -> 384;
frame_size(2, 2) -> 1152;
frame_size(2, 3) -> 576.

slot_size(1) -> 4;
slot_size(_) -> 1.

bit_rate(Id, Layer, 0) -> throw({{'badarg', 'mask'}, #{mpeg => Id, layer => Layer, bit_rate_mask => 0}});
bit_rate(2, 3, BitRateMask) -> bit_rate(2, 2, BitRateMask);

bit_rate(2, 2, Mask) when Mask =< 2#1000 -> Mask * 8;
bit_rate(2, 2, Mask)                     -> Mask * 16 - 128;

bit_rate(1, 1, Mask)                     -> Mask * 32;

bit_rate(1, 3, Mask) when Mask =< 2#0101 -> Mask * 8 + 24;
bit_rate(1, 3, Mask) when Mask =< 2#1001 -> Mask * 16 - 16;
bit_rate(1, 3, Mask) when Mask < 2#1110  -> (Mask - 2#1001) * 32 + 128;
bit_rate(1, 3, 2#1110)                   -> 320;

bit_rate(_Id, _Layer, 2#0001)            -> 32;
bit_rate(Id, Layer,   2#0010)            -> 48;
bit_rate(Id, Layer,   2#0011)                                   -> 56;

bit_rate(_, _, Mask) when Mask =< 2#0111 -> (Mask - 2#0100) * 16 + 64;

bit_rate(1, 2, Mask) when Mask =< 2#1011 -> (Mask - 2#1000) * 32 + 128;
bit_rate(1, 2, Mask)                     -> (Mask - 2#1100) * 64 + 256;

bit_rate(2, 1, Mask) when Mask =< 2#1011 -> (Mask - 2#0100) * 16 + 64;
bit_rate(2, 1, Mask)                     -> (Mask - 2#1100) * 32 + 192;

bit_rate(Id, Layer, Mask) -> throw({{'badarg', 'mask'}, #{mpeg => Id, layer => Layer, bit_rate_mask => Mask}}).

sample_rate(1, 2#00) -> 44100;
sample_rate(1, 2#01) -> 48000;
sample_rate(1, 2#11) -> 32000;
sample_rate(2, 2#00) -> 22050;
sample_rate(2, 2#01) -> 24000;
sample_rate(2, 2#11) -> 16000.

padding_size(1) -> 4;
padding_size(2) -> 1;
padding_size(3) -> 1.

frame_len(1, Padding, BitRate, SampleRate) ->
  lager:debug("padding ~p", [Padding]),
  (48 * BitRate) rem SampleRate + Padding * 4;
frame_len(Layer, Padding, BitRate, SampleRate) when Layer =:= 2 orelse Layer =:= 3 ->
  lager:debug("padding ~p", [Padding]),
  (144 * BitRate) rem SampleRate + Padding.
