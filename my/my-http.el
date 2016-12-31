(provide 'my-http)


(url-retrieve "http://google.com"
 (lambda (status) (message "%s" (buffer-string))))
