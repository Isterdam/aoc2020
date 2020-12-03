import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;

/*
Finds three integers in a given list that sum to 2020, and multiplies them
*/

public class Task2 {

	public static void main(String[] args) throws IOException {
		Task2 t2 = new Task2();
		ArrayList<Integer> nums = new ArrayList<>();

		String filePath = "C:\\Users\\AdrianMITX\\Desktop\\dev\\aoc2020\\Day1\\input.txt";
		Files.lines(Paths.get(filePath)).forEach(s -> nums.add(Integer.parseInt(s)));

		Collections.sort(nums);
		int product = 1;
		int [] triple = t2.findTriple(nums, 2020);
		for (int i : triple) {
			product *= i;
		}
		System.out.println(product);
	}

	private int[] findTriple(ArrayList<Integer> nums, int sum) {
		for (int i = 0; i < nums.size(); i += 1) {
			ArrayList<Integer> copy = new ArrayList<>(nums);
			copy.remove(i);

			int[] tuple = findTuple(copy, sum - nums.get(i));
			if (tuple != null) {
				return new int[] {nums.get(i), tuple[0], tuple[1]};
			}
		}

		return null; // couldn't find a triple
	}

	// sorted list enables time complexity O(nlog(n))
	private int[] findTuple(ArrayList<Integer> nums, int sum) {
		int low = 0;
		int high = nums.size() - 1;

		while (low < high) {
			if (nums.get(low) + nums.get(high) == sum) {
				return new int[] {nums.get(low), nums.get(high)};
			}

			if (nums.get(low) + nums.get(high) < sum) {
				low += 1;
			} else {
				high -= 1;
			}
		}

		return null; // couldn't find tuple
	}
}
