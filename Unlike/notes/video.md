    avconv -r 25 -i moor_8024_out/moor_8024-out-%05d.jpg -vf fade=type=in:start_frame=0:nb_frames=60,fade=type=out:start_frame=11324:nb_frames=60 moor_out.mp4
    