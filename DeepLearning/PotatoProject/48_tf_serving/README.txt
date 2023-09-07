
Using port 8601 (this can change)
de

PS C:\Users\PC> docker run -it -v C:\Users\Public\lmaaya\Projects\DeepLearning\PotatoProject\48_tf_serving:/48_tf_serving -p 8600:8600 --entrypoint /bin/bash tensorflow/serving