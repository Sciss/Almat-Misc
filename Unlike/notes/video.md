    avconv -r 25 -i moor_8024_out/moor_8024-out-%05d.jpg -vf fade=type=in:start_frame=0:nb_frames=60,fade=type=out:start_frame=11324:nb_frames=60 moor_out.mp4
    
rc blurry example

    avconv -i moor_out.mp4 -ss 00:03:19 -t 00:00:13 -vf fade=type=in:start_frame=4975:nb_frames=25,fade=type=out:start_frame=5284:nb_frames=25 -codec h264 -s 960x540 moor_ex_blur.mp4

