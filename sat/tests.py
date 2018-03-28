from django.test import TestCase

from .utils import partition



class PartitionTest(TestCase):
    def test_partition_with_empty_list(self):
        predicate = lambda x: True
        expected = ([], [])
        self.assertEqual(partition([], predicate), expected)

    def test_partition_with_always_true_predicate(self):
        predicate = lambda x: True
        expected = ([1,2,3], [])
        self.assertEqual(partition([1,2,3], predicate), expected)

    def test_partition_with_always_false_predicate(self):
        predicate = lambda x: False
        expected = ([], [1,2,3])
        self.assertEqual(partition([1,2,3], predicate), expected)

    def test_partition_with_predicate_filtering(self):
        predicate = lambda x: x > 3
        expected = ([], [1,2,3])
        self.assertEqual(partition([1,2,3], predicate), expected)

