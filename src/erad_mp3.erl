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
  io:format("reading frame~n"),
  MpegId = mpeg_id(MpegIdMask),
  Layer = layer(LayerMask),
  BitRate = bit_rate(MpegId, Layer, BitRateMask) * 1000,
  SampleRate = sample_rate(MpegId, SampleRateMask),
  FrameSize = frame_size(MpegId),
  FrameLen = frame_len(Layer, Padding, BitRate, SampleRate),
%  FrameLen = case {(FrameSize * BitRate) div SampleRate,
%                   (FrameSize * BitRate) rem SampleRate,
%                   Padding
%                  }
%              of
%                {FL, 0, _} -> FL;
%                {FL, _, 0} -> FL;
%                {FL, _, 1} -> FL + 1
%              end,
  io:format("frame size  ~p~n", [FrameSize]),
  io:format("bit rate    ~p~n", [BitRate]),
  io:format("sample rate ~p~n", [SampleRate]),
  io:format("frame len   ~p~n", [FrameLen]),
  case file:read(File, FrameLen) of
    {'ok', Data} -> {'frame', Data};
    Else -> Else
  end;
read_frame(File, <<"TAG+">>) ->
  io:format("reading id3v1~n"),
  case file:read(File, 227 - 4) of
    {'ok', Data} ->
      {'id3', Data};
    Else ->
      Else
  end;
read_frame(File, <<"TAG", Byte:8>>) ->
  io:format("reading id3~n"),
  case file:read(File, 128 - 4) of
    {'ok', Data} ->
      {'id3', <<Byte:8, Data/binary>>};
    Else ->
      Else
  end;
read_frame(File, <<"ID3", VerH:8>>) ->
  io:format("reading id3v2~n"),
  case file:read(File, 10 - 4) of
    {'ok', <<VerL:8, Flags:8, 0:1, S1:7, 0:1, S2:7, 0:1, S3:7, 0:1, S4:7>>} ->
      <<_E:4, Size:24>> = <<S1:7, S2:7, S3:7, S4:7>>,
      io:format("id3v2 size is ~p (~p)~n", [Size, _E]),
      case file:read(File, Size) of
        {'ok', Data} ->
          {'id3', <<VerH:8, VerL:8, Flags:1, Size:4, Data/binary>>};
        Else ->
          Else
      end;
    Else ->
      Else
  end.

mpeg_id(2#00) -> throw({{'badarg', 'mpeg_id'}, 2.5});
mpeg_id(2#10) -> 2;
mpeg_id(2#11) -> 1.

layer(2#01) -> 3;
layer(2#10) -> 2;
layer(2#11) -> 1.

frame_size(1) -> 1152;
frame_size(2) -> 576.

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

frame_len(1, Padding, BitRate, SampleRate) ->
  io:format("padding ~p~n", [Padding]),
  (48 * BitRate) rem SampleRate + Padding * 4;
frame_len(Layer, Padding, BitRate, SampleRate) when Layer =:= 2 orelse Layer =:= 3 ->
  io:format("padding ~p~n", [Padding]),
  (144 * BitRate) rem SampleRate + Padding.
